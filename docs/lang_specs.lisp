(def adtype 
	(List a')
		(Nil (Cons a' (List a'))))

TAG List(a') {

	Nil : untyped.
	Cons : [a' * [List a']].
}

(def lst (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))))

(println (length lst))

(def adtype
	(Option a')
		(None (Some a')))

TAG Option(a') {

	None : untyped.
	Some : [a'].
}

;; tau(length) = (List a') -> Integer
;; Pattern matching with Algebraic data-types (No ocmposed one but simpler-one)
(def length (ys)
	(match ys 
		(Nil 			0)
		((Cons _ xs)	(+ 1 (length xs)))))


FUNCTION length(ys) {

	IF MATCH(ys, Nil) THEN BEGIN
		mov ret.int 0
	END

	ELSE IF MATCH(ys, [Cons WILDCARD xs]) THEN BEGIN
		mov r1 1
		push.ref xs 	; Or push.val
		call length
		catch ret.int 	; Where the returned value is stored
		add r1 ret.int
		mov ret.int r1
	END

	ret ret.int
}

(def adtype
	(Bst a')
		((Leaf a') (Node (Bst a') a' (Bst a'))))

TAG Bst(a') {

	Leaf : [a'].
	Node : [[Bst a'] a' [Bst a']].
}

(def mem_bst (x bst)
	(match bst 
		((Leaf) #f)
		((Node (left k) itm (right _)) 
			(cond 
				((< x k) (mem_bst x left))
				((= x k) #t)
				(otherwise (mem_bst x right))))))

(def adtype
	(Expression)
		((Value Integer)
		(Add Expression Expression)
		(Sub Expression Expression)
		(Mul Expression Expression)
		(Div Expression Expression)
		(Mod Expression Expression)))

(def eval (expr)
	(match expr 
		((Value x)	x)
		((Add x y)	(+ (eval x) (eval y)))
		((Sub x y)	(- (eval x) (eval y)))
		((Mul x y)	(* (eval x) (eval y)))
		((Div x y)	(/ (eval x) (eval y)))
		((Mod x y)	(% (eval x) (eval y)))))

TAG {
	NAME: Expression

	CASE 
		Value : int.
		Add : [Expression * Expression].
		Sub : [Expression * Expression].
		Mul : [Expression * Expression].
		Div : [Expression * Expression].
		Mod : [Expression * Expression].
}

FUNCTION eval(expr : ref) {

	IF MATCH(expr, [Value x]) THEN BEGIN
		mov ret.int x
	END

	ELSE IF MATCH(expr, [Add x y]) THEN BEGIN
		push.ref x
		call eval
		catch r1.int

		push.ref y
		call eval
		catch r2.int

		add r1.int r2.int
		mov ret.int r1.int 
	END

	ELSE IF MATCH(expr, [Mul x y]) THEN BEGIN
		push.ref x
		call eval
		catch r1.int

		push.ref y
		call eval
		catch r2.int

		mul r1.int r2.int
		mov ret.int r1.int 
	END

	...

	ret ret.int
}

(def.public class (Bst key val left right size)




)


(def trait Animal
	(def.public eat ())
	(def.pubic travel ()))

;; Automatic getters and setters
(def.public class (Mammal (|> Animal) (number_of_legs))

	(def.public this.number_of_legs number_of_legs)

	(def.public eat ()
		(println "Mammal eats"))

	(def.public travel ()
		(println "Mammal travels")))

(def.public class (Employee (<| Person) (name age designation salary))

	(def.private this.name name)					
	(def.private this.age age)					
	(def.private this.designation designation)		
	(def.private this.salary salary)
	(def.private this.friends nil)

	(def.public add_friend (other)
		(cons other this.friends))

	(def.public increase_salary (addition)
		(def new_emp (new Employee 
						(this.name this.age this.designation 
						(+ this.salary addition))))

		(set! this new_emp))

	(def.public change_position (new_position)
		(def new_emp (new Employee 
						(this.name this.age new_position this.salary)))

		(set! this new_emp))

	(def.public friend_with (other)

		(def aux (container)
			(cond 
				((= container nil) #f)
				((= ((car container) other)) #t)
				(friend_with (cdr other)))

		(aux other)))

	(def.public print_employee ()

		(def print_list (container)
			(cond 
				((nil? container) "")
				((
					(concat 
						(car container) " " (print_list (cdr container)))))))

		(println (concat "name: " this.name))
		(println (concat "age:" " " this.age))
		(println (concat "destination: " this.destination))
		(println (concat "salary: " this.salary))

		(println "friends:")
		(println (print_list this.friends))))

;; Comment

(set! object1 (new Employee ("Alberto" 23 "Manager" 3425234.243)))

(println (object1.factorial n))
(println (object1.get_name))


(def dict {:a 4 :b 5 :c 6})

(def env 
	{
		:v1 (+ 45 12 89) 
		:v2 (- 56 43 12)
	})

(def add_to_evn (env key value)
	(set! env' env)
	(set! env' (env' :key value))
	(env'))

(def update_env (env key new_val)
	(add_to_evn (env key value)))

; Retrieve from Hashmap
(println (dict :a))
(println (+ (dict :b) (dict :b))) 

; Set a value in Hashmap
(set! dict (dict :r 6))

; Dynamic selection of Data Structures
; Implement all DS in Java Collection

; CSP for concurrency
; No iteration, just recursion
; Tail recursion optmization
; Algebraic data types
; Interfaces, Inheritance, Multiple.Inheritance
; Virtual Machine and Interpreter
; Type inference
