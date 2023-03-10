# Интерпетатор на Scheme

### Въведение
Това е проект, който реализира интерпретатор на езика Scheme. Интерпретаторът поддържа най-важните функционалности, сред които:
- променливи, числа и символи
- основни математически операции: `+`, `-`, `*`, `\`, `<`, `>`, `=`, `quotient`, `remainder`
- основни логически операции: `not`, `and`, `or`
- основни предикати: `odd?`, `even?`, `null?`, `number?`, `boolean?`, `list?`
- основни операции с двойки и списъци: `cons`, `car`, `cdr`, `list`
- специални форми: `define`, `if`, `cond`
- `lambda`

### Начин на употреба
Интерпретаторът се използва чрез извикване на функцията `interpret` с аргумент - списък от изрази (S-expressions). При повече от един израз в списъка, изразите ще бъдат изчислени в реда на изписването им. 
Използването на интерпретатора е сходно с използването на REPL на Scheme (например в средата DrRacket), но все пак са необходими някои насоки:
- подаването на празен списък от изрази за интерпретиране ще бъде отхвърлено:
```scheme
> (interpret '())
Nothing to evaluate
```
- бъдете сигурни, че кодът Ви се интерпретира от интерпретатора, а не от Scheme:
  - `(interpret a)` оценява `a` в Scheme и едва тогава интерпретаторът оценява вече оценената стойност (неправилна употреба) 
  - `(interpret 'a)` се оценява единствено от интерпретатора (правилна употреба)
  - `(interpret '(a))` също е правилна употреба - интепретаторът, също както в горния пример, ще потърси стойността в средата и ще върне стойността ѝ (ако има такава)
- интерпретаторът работи и с множество изрази:
```scheme
> (interpret '(
                  (define (f x) (+ x 2))
                  (define x 5)
                  ((if (null? '()) f g) x)))
7
```
- по подразбиране интерпретаторът не работи с квадратни скоби:
```scheme
> (interpret '[(define (f x) (+ x 2))
                  (define x 5)
                  ((if (null? '()) f g) x)])
read-syntax: illegal use of `[`
```
- интерпретаторът не покрива всички сценарии на некоректен вход
### Имплементация на проекта
Реализацията на проекта премина през няколко етапа, които смислово разделят кода на секции:
#### Имплементация на `interpret`. Разглеждане на случаи за входа
За да бъде интерпретиран потребителският вход (или изобщо даден израз), той трябва да бъде разделен на смислови части, всяка от които трябва да бъде интерпретирана поотделно. За целта отделих следните сценарии:
- число или булев израз
- специална форма - `and`, `or`, `cond`, `list`, `if`, `define`, `set`
- функция, дефинирана от потребителя
- списък от изрази
- ламбда израз
- други изрази извън тези категории

#### Имплементация и работа със среди
При създаването на имплементацията за интерпретиране на ламбда изрази се появи сериозна пречка: необходимостта от дефиниране и използване на среди, която осъзнах чрез следния пример:
```scheme
(define x 5)
(lambda (x)(+ x 1))
```
Средите са необходим инструмент за определяне на стойността на един функционален обект, спрямо контекста, в който се намира. В примера по-горе, без коцепцията за среда, има опасност символът `x` да бъде възприет като вече дефинираната променлива, а не като името на параметър на анонимната функция. 
За целта функцията `interpret` бе пренаписана да приема втори аргумент - функция, която връща стойността на функционален обект в текущата среда (`get-from-environment`). Нуждата от начин за съпоставяне на име и стойност автоматично наложи употребата на структурата от данни асоциативен списък за концепцията "среда". 
Въвеждането на среди доведе до необходимостта от дефиниране на основните функционални обекти в началната среда (празен асоциативен списък) - тоест "разширение на средата". Затова беше създадена функцията `extend-environment`, която по подразбиране разширява началната среда с често срещани процедури (но не и специалните форми, които се интерпретират по различен начин чрез `evaluate-special-form`, `evaluate-define`, `evaluate-set!`).  
В резултат функцията за интерпретиране изисква два аргумента, а по условие `interpret` получава само един. Затова основната логика остана във функцията `evaluate`, a `interpret` се използва само като wrapper. 
### Външни източници
При създаването на този проект използвах за справка [тази лекция](https://www.youtube.com/watch?v=OyfBQmvr2Hc). Тя беше безценен помощник, който ми даде представа какво трябва да съдържа един интерпретатор на Scheme и откъде трябва да започна. В допълнение, тя ми помогна да осъзная неизбежността от създаването на среди (което се надявах по някакъв начин да избегна, преди да започна работа по проекта). От нея заимствах и идеята за интерпретиране на ламбда и случаят `(operator operand)`. 
### Възможности за подобрение
- дефиниране на други "вградени" функции на интерпретатора
- използване на собствена имплементация на някои функции, вместо използване на имплементация на Scheme "наготово". Например:
  ```scheme
  (define (map f l)
    (if (null? l)
        l
        (cons (f (car l)) (map f (cdr l)))))
  ```
- обработване на грешки: в началото беше замислено да бъдат обработвани някои грешки чрез функцията `error`, която в R5RS се имплементира така: 
  ```scheme
  (define error #f)
      (call-with-current-continuation (lambda (k)
                (set! error
                  (lambda error-arguments
                    (display ">>>> ERROR ")
                    (newline)
                    (k error-arguments)))
                'done)) 
  ```
  В процеса на работа не разбрах употребата на `call-with-current-continuation` и се отказах от обработката на грешки, за да не използвам в решението си код, който не знам как работи. 
- разделяне на кода на модули и файлове - резултатният код е дълъг и се опитах да го разделя на два файла - един за работата със среди, другият - за останалата част. Това доведе до циклична зависимост (circular dependency), която не можах да разреша. 
- реализиране само с `lambda`. 
- имплементиране на флагове и параметри за използване на интерпретатора
