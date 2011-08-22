class Array
	# returns all but the last n + 1 elements
	def tail(n=0)
		return [] if (n + 1) > length
		last(length - (n + 1))
	end

	def to_hash
		Hash[self.flatten]
	end

	def second
		self[1]
	end

	def third
		self[2]
	end
end

class Lexer
	def lex(string)
		# This is one of the most hackish things I've ever written, but it works
		# well enough.
		string.gsub('(', ' ( ').gsub(')', ' ) ').split(' ')
	end
end

class Parser
	def parse(tokens)
		_parse(tokens, []).first
	end

	protected

		def _parse(tokens, list)
			case tokens.first
				when '('
					parsed, rest = _parse(tokens.tail, [])
					_parse(rest, list << parsed)
				when ')'
					[list, tokens.tail]
				when nil # empty list
					[list, []]
				else
					_parse(tokens.tail, list << tokens.first)
			end
		end
end

class Interpreter
	class Environment
		def initialize(parent)
			@parent = parent
			@bindings = {}
		end

		def define(name, value)
			@bindings[name] = value
			return value
		end

		def extend(bindings)
			env = Environment.new(self)
			bindings.each {|k,v| env.define(k,v) }
			return env
		end

		def lookup(name)
			@bindings[name] || @parent.lookup(name)
		end

		def method_missing(*args)
			@parent.send(*args)
		end
	end

	class RootEnvironment < Environment
		def initialize
			@parent = nil

			@bindings = {
				'+' => :add,
				'*' => :multiply,
				'/' => :divide,
				'-' => :subtract,
				'quit' => :quit,
				'display' => :str_display,
				"print" => :str_print,
				"format" => :format,
				"eq?" => :eq?,
				"error" => :error,
				"lambda?" => :lambda?,
			}
		end

		def lookup(name)
			@bindings[name] || name
		end

		def quit
			exit
		end

		def eq?(first, second)
			return "#t" if first == second
			return "#f"
		end

		def add(*args)
			args.inject(0) do |acc,x|
				acc + x.to_i
			end.to_s
		end

		def subtract(*args)
			acc = args.shift.to_i
			args.inject(acc) do |acc,x|
				acc - x.to_i
			end.to_s
		end

		def multiply(*args)
			args.inject(1) do |acc,x|
				acc * x.to_i
			end.to_s
		end

		def divide(numerator, denominator)
			return (numerator.to_i / denominator.to_i).to_s
		end

		def str_display(*args)
			string = format(*args)
			puts string
			return string
		end

		def str_print(*args)
			string = format(*args)
			print string
			return string
		end

		def error(*args)
			raise RuntimeError.new(format(*args))
		end

		def format(*args)
			args.map {|arg| ast_to_string(arg)}.join('')
		end

		def lambda?(arg)
			eq? arg.is_a?(Lambda), true
		end
	end
	Root = RootEnvironment.new

	class Lambda
		def initialize(args, body, env, interpreter)
			@args = args
			@body = body
			@env = env
			@interpreter = interpreter
		end

		def apply(values)
			bindings = Hash[*@args.zip(values).flatten]
			newEnv = @env.extend(bindings)
			
			@interpreter.evaluate_seq(@body, newEnv)
		end

		def to_s
			"(lambda (#{@args.join(',')}) #{ast_to_string(@body)})"
		end
	end

	def evaluate(expression, env=Root)
		if not expression.is_a? Array
			env.lookup(expression)
		else
			case expression.first
				when "lambda"
					# lambda definition
					args = expression.second
					body = expression.tail(1)
					Lambda.new(args, body, env, self)
				when "define"
					# constant definition
					name = expression.second

					# handle (define (fun <args>) <body>) syntax
					if name.is_a? Array
						args = name.tail
						name = name.first
						body = expression.tail(1)

						value = Lambda.new(args, body, env, self)
					else
						value = evaluate(expression.third, env)
					end

					env.define(name, value)
				when "if"
					evaluate_if(expression.tail, env)
				when "quote"
					expression.tail
				when "cond"
					evaluate(cond_to_if(expression.tail), env)
				when "begin"
					evaluate_seq(expression.tail, env)
				else
					# Procedure evaluation
					procedure = expression.first
					exprs = expression.tail

					fun = evaluate(procedure, env)
					args = exprs.map {|e| evaluate(e, env)}

					case fun
						when Symbol
							env.send(fun, *args)
						when Lambda
							fun.apply(args)
						else
							raise RuntimeError.new("No such function: #{fun}")
					end
			end
		end
	end

	def evaluate_seq(seq, env=Root)
		seq.map {|s| evaluate(s, env)}.last
	end

	def evaluate_if(seq, env)
		def predicate(seq)
			seq.first
		end

		def consequent(seq)
			seq.second
		end

		def alternative(seq)
			seq.third
		end

		res =
			if true? evaluate(predicate(seq), env)
				consequent(seq)
			else
				alternative(seq)
			end

		evaluate(res, env)
	end

	def sequence_to_expr(expr)
		["begin"] + expr
	end

	def cond_to_if(clauses)
		def cond_predicate(cond)
			cond.first
		end

		def cond_actions(cond)
			cond.tail
		end

		def cond_else_clause?(cond)
			cond_predicate(cond) == "else"
		end

		def make_if(predicate, consequent, alternative)
			["if", predicate, consequent, alternative]
		end

		if clauses.length == 0
			return false
		else
			first = clauses.first
			rest = clauses.tail

			if cond_else_clause? first
				sequence_to_expr(cond_actions(first))
			else
				make_if cond_predicate(first), sequence_to_expr(cond_actions(first)), cond_to_if(rest)
			end
		end
	end

	def true?(value)
		return false if value == "#f"
		return true if value == "#t"
	end
end

def ast_to_string(ast)
	string = ""
	case ast
		when Array
			string << "(#{ast.map {|node| ast_to_string(node) }.join(" ")})"
		when Interpreter::Lambda
			string << ast.to_s
		else
			string << ast
	end

	return string
end

class REPL
	@lexer = Lexer.new
	@parser = Parser.new
	@interpreter = Interpreter.new

	def self.init
		loop do
			print ">> "
			input = gets
			run("(display #{input})")
		end
	end

	def self.run(input)
		@interpreter.evaluate_seq(@parser.parse(@lexer.lex(input)))
	end

	# Commonly used functions that don't need to be implemented in ruby
	LIBRARY = <<-EOL
		(define exit quit)

		(define (true? x)
			(eq? x #t))
		(define (false? x) (not (true? x)))
		(define (not x)
			(if (true? x)
				#f
				#t))

		(define (cons a b)
			(lambda (cmd)
				(if (eq? cmd 'car)
						a
						(if (eq? cmd 'cdr)
							b
							(error "no such method -- " cmd " -- CONS")))))
		(define car (lambda (x)
			(x 'car)))
		(define cdr (lambda (x)
			(x 'cdr)))
		(define cadr (lambda (x) (car (cdr x))))
	EOL
	run(LIBRARY)

	# Some tests to make sure things work properly
	TESTS = <<-EOL
		(define (run-tests)

			(define (assert x y)
				(if (true? x)
					(print .)
					(error y)))

			(define (deny x y)
				(if (false? x)
					(print .)
					(error y)))

			(define (environment-tests)
				(define (inherit-local-over-global-test)
					(define b 1)
					(define (a) b)
					(define (c b)
						(lambda () (a)))
					(assert (eq? 1 ((c 2))) "b should reference its definition when defined"))
				(inherit-local-over-global-test))

			(define (logic-tests)
				(define (true-test)
					(assert (eq? #t (true? #t)) "#t is true")
					(deny (eq? #t (true? #f)) "#f is not true"))
				(define (not-test)
					(assert (eq? #t (not #f)) "not should negate #f")
					(assert (eq? #f (not #t)) "not should negate #t"))
				(true-test)
				(not-test))

			(define (if-tests)
				(define (if-test)
					(assert (eq? #f (if #f
						#t
						#f)) "if should return the else clause for a false predicate"))
				(if-test))

			(define (cond-tests)
				(define (cond-test)
					(assert (eq? #t (cond (#f #f)
																(else #t)))))
				(cond-test))

			(define (cons-tests)
				(define (car-cons-test)
					(assert (eq? 1 (car (cons 1 2))) "car-cons should return the car of a pair"))
				(define (cdr-cons-test)
					(assert (eq? 2 (cdr (cons 1 2))) "cdr-cons should return the cdr of a pair"))
				(car-cons-test)
				(cdr-cons-test))

			(environment-tests)
			(logic-tests)
			(if-tests)
			(cond-tests)
			(cons-tests))
	EOL
	run(TESTS)
end

if ARGV.length == 0
	REPL.init
else
	ARGV.each do |arg|
		if File.exist? arg
			command = File.read(arg)
			REPL.run(command)
		else
			command = "(display #{arg})"
			REPL.run(command)
		end
	end
end