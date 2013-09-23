;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robots) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require
 "extras.rkt")

(provide
 initial-robot
 robot-left 
 robot-right
 robot-forward
 robot-south? 
 robot-north? 
 robot-west? 
 robot-east?) 

(define RADIUS 15)
;; RADIUS is a the size of the Robot in cm
;; It is measured from the center of the robot
(define MAX-X-AXIS 200)
;; MAX-X-AXIS is the max length the robot can 
;; travel from west to east in cm
(define MAX-Y-AXIS 400)
;; MAX-X-AXIS is the max length the robot can
;; travel south to north in cm
(define MIN-X-Y 0)
;; MIN-X-Y is the lowest west and south point possible.

(define-struct robot (x-axis y-axis direction))

;; A Robot is a (make-robot x-axis y-axis)
;; Interp:
;; Robot is a circular robot with fixed radius 15 cm.
;; initially the robot will be facing the direction north
;; (x-axis, y-axis) is the point where the robot 
;; has to be created
;; direction is the direction which the robot faces

;; template
;; robot-fn : Robot -> ??
(define (robot-fn rbt)
  (... (robot-xaxis rbt)
       (robot-yaxis rbt)
       ))

;; initial-robot : Number Number -> Robot
;; GIVEN: a set of (x,y) coordinates
;; RETURNS: a robot with its center at those coordinates, facing north
;; (up).

;; Examples
;; (initial-robot 180 200) => (make-robot 180 200 "north")

;; STRATEGY: domain knowledge
(define (initial-robot xaxis yaxis)
  (make-robot xaxis yaxis "north"))

(check-expect (and (= (robot-xaxis (initial-robot 180 200)) 180)
                   (= (robot-xaxis (initial-robot 180 200)) 180)
                   (robot-north? (initial-robot 180 200))) true)

;; robot-south? : Robot -> Boolean
;; GIVEN: A robot
;; RETURNS: true iff the robot faces the direction south
;; EXAMPLES:
;; (robot-north? (make-robot 20 20 "north")) => false
;; (robot-north? (make-robot 20 20 "south")) => true

;; Strategy: structural decomposition on rbt : Robot
(define (robot-south? rbt)
  (string=? (robot-direction rbt) "south"))

;; Testcases
(check-expect (robot-south? (make-robot 20 20 "south")) true) 
(check-expect (robot-south? (make-robot 20 20 "north")) false) 

;; robot-north? : Robot -> Boolean
;; GIVEN: A robot
;; RETURNS: true iff the robot faces the direction north
;; EXAMPLES:
;; (robot-north? (make-robot 20 20 "north")) => true
;; (robot-north? (make-robot 20 20 "south")) => false

;; Strategy: structural decomposition on rbt : Robot
(define (robot-north? rbt )
  (string=? (robot-direction rbt) "north"))

;; Testcases
(check-expect (robot-north? (make-robot 20 20 "north")) true) 
(check-expect (robot-north? (make-robot 20 20 "south")) false) 

;; robot-west? : Robot -> Boolean
;; GIVEN: A robot
;; RETURNS: true iff the robot faces the direction west
;; EXAMPLES:
;; (robot-west? (make-robot 20 20 "north")) => false
;; (robot-west? (make-robot 20 20 "west")) => true

;; Strategy: structural decomposition on rbt : Robot
(define (robot-west? rbt )
  (string=? (robot-direction rbt) "west"))

;; Testcases
(check-expect (robot-west? (make-robot 20 20 "west")) true) 
(check-expect (robot-west? (make-robot 20 20 "north")) false) 

;; robot-east? : Robot -> Boolean
;; GIVEN: A robot
;; RETURNS: true iff the robot faces the direction east
;; EXAMPLES:
;; (robot-east? (make-robot 20 20 "north")) => false
;; (robot-east? (make-robot 20 20 "east")) => true

;; Strategy: structural decomposition on rbt : Robot
(define (robot-east? rbt )
  (string=? (robot-direction rbt) "east"))

;; Testcases
(check-expect (robot-east? (make-robot 20 20 "east")) true) 
(check-expect (robot-east? (make-robot 20 20 "north")) false) 

;; robot-xaxis : Robot -> Number
;; GIVEN: A robot
;; RETURNS: returns the x-axis of the robots current position
;; EXAMPLES:
;; (robot-xaxis (make-robot 20 50 "east")) => 20
;; (robot-xaxis (make-robot 200 20 "east")) => 200

;; Strategy: structural decomposition on rbt : Robot
(define (robot-xaxis rbt)
  (robot-x-axis rbt))

;; Testcases
(check-expect (robot-xaxis (make-robot 20 50 "north")) 20)
(check-expect (robot-xaxis (make-robot 200 20 "north")) 200)

;; robot-yaxis : Robot -> Number
;; GIVEN: A robot
;; RETURNS: returns the y-axis of the robots current position
;; EXAMPLES:
;; (robot-xaxis (make-robot 20 50 "east")) => 20
;; (robot-xaxis (make-robot 200 20 "east")) => 200

;; Strategy: structural decomposition on rbt : Robot
(define (robot-yaxis rbt)
  (robot-y-axis rbt))

;; Testcases
(check-expect (robot-yaxis (make-robot 20 50 "north")) 50)
(check-expect (robot-yaxis (make-robot 200 20 "north")) 20)

;; south-limit-exceeded? : Robot PosNum-> Boolean
;; GIVEN: A robot and the distance it has to move
;; RETURNS: returns if it has gone over the allowed range in x-axis
;; EXAMPLES:
;; (south-limit-exceeded? (make-robot 50 30 "north") 2) => true
;; Border case
;; (south-limit-exceeded? (make-robot 50 17 "south") 2) => true 
;; (south-limit-exceeded? (make-robot 50 30 "south") 15) => true
;; Strategy: Functional composition

(define (south-limit-exceeded? rbt distance)
  (if (or (< (-(-(robot-yaxis rbt) RADIUS) distance) MIN-X-Y)
          (> (-(-(robot-yaxis rbt) RADIUS) distance) MAX-Y-AXIS))
      true false))

;; Testcases
(check-expect (south-limit-exceeded? (make-robot 50 17 "south") 2) false)
(check-expect (south-limit-exceeded? (make-robot 50 17 "south") 3) true)
(check-expect (south-limit-exceeded? (make-robot 50 30 "south") 2) false)
(check-expect (south-limit-exceeded? (make-robot 50 30 "south") 15) false)
(check-expect (south-limit-exceeded? (make-robot 50 30 "south") 16) true)
(check-expect (south-limit-exceeded? (make-robot -100 -100 "south") 15) true)

;; north-limit-exceeded? : Robot PosNum-> Boolean
;; GIVEN: A robot and the distance it has to move
;; RETURNS: returns if it has gone below the x-axis allowed range
;; EXAMPLES:
;; (north-limit-exceeded? (make-robot 50 30 "north") 75) => false
;; (north-limit-exceeded? (make-robot 50 385 "north") 15) => true
;; Border Case
;; (north-limit-exceeded? (make-robot 50 398 "north") 2) => true

;; Strategy: Functional composition

(define (north-limit-exceeded? rbt distance)
  (if (or (> (+ (+ (robot-yaxis rbt) RADIUS) distance) MAX-Y-AXIS)
          (< (+ (+ (robot-yaxis rbt) RADIUS) distance) MIN-X-Y)
          ) true false))

;; Testcases
(check-expect (north-limit-exceeded? (make-robot 50 383 "north") 2) false)
(check-expect (north-limit-exceeded? (make-robot 50 30 "north") 75) false)
(check-expect (north-limit-exceeded? (make-robot 50 390 "north") 15) true)

;; west-limit-exceeded? : Robot PosNum-> Boolean
;; GIVEN: A robot and the distance it has to move
;; RETURNS: returns if it has gone over the y-axis allowed range
;; EXAMPLES:
;; (west-limit-exceeded? (make-robot 150 198 "west") 50) => true

;; Strategy: Functional composition
(define (west-limit-exceeded? rbt distance)
  (if (or (> (+ (+ (robot-xaxis rbt) RADIUS) distance) MAX-X-AXIS)
          (< (+ (+ (robot-xaxis rbt) RADIUS) distance) MIN-X-Y)
          )true false))


;; Testcases
(check-expect (west-limit-exceeded? (make-robot 150 198 "west") 50) true)
(check-expect (west-limit-exceeded? (make-robot 150 30 "west") 25) false)
(check-expect (west-limit-exceeded? (make-robot 100 185 "west") 85) false)

;; east-limit-exceeded? : Robot PosNum-> Boolean
;; GIVEN: A robot and the distance it has to move
;; RETURNS: returns if it has gone below the x-axis allowed range
;; EXAMPLES:
;; (east-limit-exceeded? (make-robot 50 198 "east") 35) => false

;; Strategy: Domain Knowlege
(define (east-limit-exceeded? rbt distance)
  (if (or (< (- (- (robot-xaxis rbt) RADIUS) distance) MIN-X-Y) 
          (> (- (- (robot-xaxis rbt) RADIUS) distance) MAX-X-AXIS)
          )true false))

;; Testcases
(check-expect (east-limit-exceeded? (make-robot 50 198 "east") 35) false)
(check-expect (east-limit-exceeded? (make-robot 100 30 "east") 50) false)
(check-expect (east-limit-exceeded? (make-robot 125 185 "east") 110) false)

;; robot-inside-plane? : Robot PosNum-> Boolean
;; GIVEN: A robot and the distance it has to move
;; RETURNS: returns if it is inside the given plane
;; EXAMPLES:
;; (robot-inside-plane? (make-robot 10 30 "south")) => false
;; (robot-inside-plane? (make-robot 100 30 "east")) => true

;; Strategy: Functional Composition
(define (robot-inside-plane? rbt)
  (and
   (not (south-limit-exceeded? rbt 0))
   (not (north-limit-exceeded? rbt 0))
   (not (west-limit-exceeded? rbt 0))
   (not (east-limit-exceeded? rbt 0))))

(check-expect (robot-inside-plane? (make-robot 10 30 "south")) false)
(check-expect (robot-inside-plane? (make-robot 100 5 "south")) false)
(check-expect (robot-inside-plane? (make-robot 100 390 "north")) false)
(check-expect (robot-inside-plane? (make-robot 100 30 "north")) true)
(check-expect (robot-inside-plane? (make-robot 190 30 "west")) false)
(check-expect (robot-inside-plane? (make-robot 150 30 "west")) true)
(check-expect (robot-inside-plane? (make-robot 10 300 "east")) false)
(check-expect (robot-inside-plane? (make-robot 100 30 "east")) true)

(check-expect (robot-inside-plane? (make-robot -100 100 "south")) false)


;; robot-enters-yplane? : PosInt PosInt -> Boolean
;; GIVEN: x-axis and y-axis of the robot and the distance 
;; to be travelled
;; RETURNS: if the robot enters the given plane during movement.
;; Examples:
;; (robot-enters-yplane? (make-robot 100 30 "west") 60) => false

;; STRATEGY: Domain knowledge

(define (robot-enters-yplane? xaxis yaxis distance)
  (if ( and (> xaxis MIN-X-Y) (< xaxis MAX-X-AXIS)
            (or (< yaxis MIN-X-Y)(> yaxis MAX-Y-AXIS)))
      (if(> yaxis MAX-Y-AXIS)
         (if (< (- (- yaxis RADIUS) distance) MAX-Y-AXIS)
             true
             false)
         (if (> (+ (+ yaxis RADIUS) distance) MIN-X-Y)
             true
             false))
      false))

;; testcases

(robot-enters-yplane? xaxis yaxis distance)

;; robot-enters-yplane? : PosInt PosInt -> Boolean
;; GIVEN: x-axis and y-axis of the robot and the distance 
;; to be travelled
;; RETURNS: if the robot enters the given plane during movement.
;; Examples:
; (robot-enters-yplane? (make-robot 100 -400 "north") 1000)) => false
;; STRATEGY: Domain knowledge

(define (robot-enters-xplane? xaxis yaxis distance)
  (if ( and (> yaxis MIN-X-Y) (< yaxis MAX-Y-AXIS)
            (or (< xaxis MIN-X-Y)(> xaxis MAX-X-AXIS)))
      (if(> xaxis MAX-X-AXIS)
         (if (< (- (- xaxis RADIUS) distance) MAX-X-AXIS)
             true
             false)
         (if (> (+ (+ xaxis RADIUS) distance) MIN-X-Y)
             true
             false))
      false))

;; testcases


;; robot-forward : Robot PosInt -> Robot
;; GIVEN: A robot and the disatnce to be moved
;; RETURNS: A new robot at the given position is returned 
;; if the distance tarvelled lies within the plane 
;; else moves the robot to the closest border and returns
;; Examples:
;; (robot-yaxis (robot-forward (make-robot 100 30 "south") 20)) => 15
;; (robot-xaxis (robot-forward (make-robot 1000 200 "east") 1800)) => 15
;; STRATEGY: Functional Composition

(define(robot-forward rbt distance)
  (cond
    [(robot-south? rbt)
     (if (and (robot-inside-plane? rbt)(south-limit-exceeded? rbt distance))
         (make-robot (robot-xaxis rbt) (+ MIN-X-Y RADIUS) "south")
         (if (robot-enters-yplane? 
              (robot-xaxis rbt) (robot-yaxis rbt) distance)
             (make-robot (robot-xaxis rbt) (+ MIN-X-Y RADIUS) "south")
             (make-robot (robot-xaxis rbt)(-(robot-yaxis rbt) distance)"south")
             ))]
    [(robot-north? rbt)
     (if (and (robot-inside-plane? rbt)(north-limit-exceeded? rbt distance))
         (make-robot (robot-xaxis rbt) (- MAX-Y-AXIS RADIUS) "north")
         (if (robot-enters-yplane? 
              (robot-xaxis rbt) (robot-yaxis rbt) distance)
             (make-robot (robot-xaxis rbt) (- MAX-Y-AXIS RADIUS) "north")
             (make-robot (robot-xaxis rbt)
                         (+ (robot-yaxis rbt) distance) "north")
             ))]
    [(robot-west? rbt)
     (if (and (robot-inside-plane? rbt)(west-limit-exceeded? rbt distance))
         (make-robot (- MAX-X-AXIS RADIUS) (robot-yaxis rbt) "west")
         (if (robot-enters-xplane? 
              (robot-xaxis rbt) (robot-yaxis rbt) distance)
             (make-robot (- MAX-X-AXIS RADIUS) (robot-yaxis rbt) "west")
             (make-robot 
              (+(robot-xaxis rbt) distance) (robot-yaxis rbt) "west")
             ))]
    [(robot-east? rbt)
     (if (and (robot-inside-plane? rbt)(east-limit-exceeded? rbt distance))
         (make-robot (+ MIN-X-Y RADIUS) (robot-yaxis rbt) "east")
         (if (robot-enters-xplane? 
              (robot-xaxis rbt) (robot-yaxis rbt) distance)
             (make-robot (+ MIN-X-Y RADIUS) (robot-yaxis rbt) "east")
             (make-robot 
              (-(robot-xaxis rbt) distance) (robot-yaxis rbt) "east")
             ))]))


(check-expect (robot-yaxis (robot-forward (make-robot 100 30 "south") 20)) 15)
(check-expect (robot-yaxis (robot-forward (make-robot 100 60 "south") 20)) 40)
(check-expect (robot-yaxis (robot-forward 
                            (make-robot 100 380 "north") 20)) 385)
(check-expect (robot-yaxis (robot-forward (make-robot 100 30 "north") 20)) 50)
(check-expect (robot-xaxis (robot-forward (make-robot 175 30 "west") 20)) 185)
(check-expect (robot-xaxis (robot-forward (make-robot 100 30 "west") 60)) 160)
(check-expect (robot-xaxis (robot-forward (make-robot 20 30 "east") 20)) 15)
(check-expect (robot-xaxis (robot-forward (make-robot 100 30 "east") 20)) 80)

(check-expect (robot-xaxis (robot-forward 
                            (make-robot -100 30 "east") 20)) -120)
(check-expect (robot-yaxis (robot-forward (make-robot -100 30 "east") 20)) 30)
(check-expect (robot-yaxis (robot-forward (make-robot 20 16 "south") 20)) 15)
(check-expect (robot-yaxis (robot-forward 
                            (make-robot 100 500 "south") 800)) 15)
(check-expect (robot-yaxis (robot-forward 
                            (make-robot 600 500 "south") 800)) -300)
(check-expect (robot-yaxis (robot-forward 
                            (make-robot 100 500 "south") 50)) 450)
(check-expect (robot-yaxis (robot-forward 
                            (make-robot 100 -400 "north") 1000)) 385)
(check-expect (robot-yaxis 
               (robot-forward (make-robot 600 -400 "north") 1000)) 600)
(check-expect (robot-yaxis (robot-forward 
                            (make-robot 100 -400 "north") 50)) -350)
(check-expect (robot-xaxis (robot-forward 
                            (make-robot -100 200 "west") 800)) 185)
(check-expect (robot-xaxis (robot-forward 
                            (make-robot -100 200 "west") 50)) -50)
(check-expect (robot-xaxis (robot-forward 
                            (make-robot -100 500 "west") 800)) 700)
(check-expect (robot-xaxis (robot-forward 
                            (make-robot 1000 200 "east") 1800)) 15)
(check-expect (robot-xaxis (robot-forward 
                            (make-robot 1000 500 "east") 1800)) -800)
(check-expect (robot-xaxis (robot-forward 
                            (make-robot 1000 200 "east") 100)) 900)


;; robot-left : Robot -> Robot
;; GIVEN: a Robot
;; RETURNS: turns to the given robot to left and returns the robot.
;; Examples:

;; (robot-left (make-robot 100 30 "south")) => east
;; (robot-left (make-robot 100 30 "east")) => north

;; STRATEGY: functional composition

(define (robot-left rbt)
  (cond 
    [(robot-south? rbt) 
     (make-robot (robot-xaxis rbt) (robot-yaxis rbt) "east")]
    [(robot-north? rbt) 
     (make-robot (robot-xaxis rbt) (robot-yaxis rbt) "west")]
    [(robot-west? rbt) 
     (make-robot (robot-xaxis rbt) (robot-yaxis rbt) "south")]
    [(robot-east? rbt) 
     (make-robot (robot-xaxis rbt) (robot-yaxis rbt) "north")]))

(check-expect (robot-left (make-robot 100 30 "south")) 
              (make-robot 100 30 "east"))
(check-expect (robot-left (make-robot 100 30 "north")) 
              (make-robot 100 30 "west"))
(check-expect (robot-left (make-robot 100 30 "west")) 
              (make-robot 100 30 "south"))
(check-expect (robot-left (make-robot 100 30 "east"))
              (make-robot 100 30 "north"))

;; robot-right : Robot -> Robot
;; GIVEN: a Robot
;; RETURNS: turns to the given robot to right and returns the robot.
;; Examples:

;; (robot-right (make-robot 100 30 "south")) => west


;; STRATEGY: functional composition

(define (robot-right rbt)
  (cond 
    [(robot-south? rbt) (make-robot 
                         (robot-xaxis rbt) (robot-yaxis rbt) "west")]
    [(robot-north? rbt) (make-robot 
                         (robot-xaxis rbt) (robot-yaxis rbt) "east")]
    [(robot-west? rbt)  (make-robot 
                         (robot-xaxis rbt) (robot-yaxis rbt) "north")]
    [(robot-east? rbt)  (make-robot 
                         (robot-xaxis rbt) (robot-yaxis rbt) "south")]))

(check-expect (robot-right 
               (make-robot 100 30 "south")) (make-robot 100 30 "west"))
(check-expect (robot-right 
               (make-robot 100 30 "north")) (make-robot 100 30 "east"))
(check-expect (robot-right 
               (make-robot 100 30 "west")) (make-robot 100 30 "north"))
(check-expect (robot-right 
               (make-robot 100 30 "east")) (make-robot 100 30 "south"))