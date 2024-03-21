;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     CMP2020-2324 Snake game                            ;;
;;                                                                        ;;
;; If you find any bugs or need help with Netlogo, contact the module     ;;
;;  delivery team (e.g. by posting a message on blackboard).              ;;
;;                                                                        ;;
;; This model was based on:                                               ;;
;;  Brooks, P. (2020) Snake-simple. Stuyvesant High School. Avaliable
;;  from http://bert.stuy.edu/pbrooks/fall2020/materials/intro-year-1/Snake-simple.html
;;    [accessed 16 November 2023].                                        ;;
;;                                                                        ;;
;; Don't forget to appropriately reference the resources you use!         ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

globals [
  wall-color
  clear-colors ; list of colors that patches the snakes can enter have

  level tool ; ignore these two variables they are here to prevent warnings when loading the world/map.
]

patches-own [
  age ; if not part of a snake, age=-1. Otherwise age = ticks spent being a snake patch.
]

breed [snakes snake]
snakes-own [
  team ; either red team or blue team
  mode ; how is the snake controlled.
  snake-age ; i.e., how long is the snake
  snake-color ; color of the patches that make up the snake
  current-path ; current path being followed
]

extensions [
  table
]

;;=======================================================
;; Setup

to setup ; observer
  clear-all
  ifelse map-file = "blank" [
    ask patches [
      set age -1
      set pcolor black
    ]
    set wall-color gray
    set clear-colors [black green cyan]
  ] [
    setup-walls
    setup-snakes

    set clear-colors [black green cyan]
    ; there will alwasy be two randomly placed pieces of food within the environment:
    make-food
    make-food
  ]
  reset-ticks
end

;;--------------------------------

to setup-walls  ; observer
  ; none-wall patches are colored black:
  ask patches [
    set age -1
    set pcolor black
  ]

  set wall-color gray

  ifelse map-file = "empty" [
    ; Set the edge of the environment to the wall-color:
    ask patches with [abs pxcor = max-pxcor or abs pycor = max-pycor] [set pcolor wall-color]
  ] [  ; load the map:
    let map_file_path (word "maps/" map-file ".csv")
    ifelse (file-exists? map_file_path) [
      import-world map_file_path
    ] [
      user-message "Cannot find map file. Please check the \"maps\" directory is in the same directory as this Netlogo model."
    ]
    ; set the patch size (so that the larger maps don't cover the controls)
    ifelse map-file = "snake-map-3" [ set-patch-size 11 ]
                                    [ set-patch-size 14 ]
  ]
end

;;--------------------------------

to setup-snakes  ; observer
  ; create the red/orange snake:
  create-snakes 1 [
    set team "red" ; /orange
    set xcor max-pxcor - 1
    set color red - 2
    set snake-color red + 11
    set current-path []

    set mode red-team-mode
  ]
  ; create the blue/purple snake (but only when in two-player mode):
  if two-player[
    create-snakes 1 [
      set team "blue" ;/purple
      set xcor 0 -(max-pxcor - 1)
      set color blue - 2
      set snake-color blue + 11
      set current-path []

      set mode blue-team-mode
    ]
  ]
  ; set the attributes that are the same for both snakes:
  ask snakes [
    set heading 0
    set ycor 0
    set snake-age 2 ; i.e. body contains two patches

    ;; Create the initial snake body
    ask patch [xcor] of self  0 [set pcolor [snake-color] of myself
                                 set age 0 ]
    ask patch [xcor] of self  -1 [set pcolor [snake-color] of myself
                                  set age 1]
  ]
end

;;=======================================================

;;;
; Make a random patch green (e.g. the color of the food)
to make-food
  ask one-of patches with [pcolor = black] [
    set pcolor green
  ]
end

;;=======================================================

;;;
; Our main game control method
to go ; observer
  let loser nobody ; nobody has lost the game yet...
  let winner nobody ; nobody has won the game yet...

  ask patches with [pcolor = cyan] [set pcolor black] ;;

  ask snakes [
    ; 1. Set which direction the snake is facing:
    ;  You will want to expand the following if statement -- to call the approaches that you implement
    ifelse mode = "random" [
      face-random-neighboring-patch
    ] [
      pathfind
    ]

    ; 2. move the head of the snake forward
    fd 1

    ; 3. check for a collision (and thus game lost)
    if not member? pcolor clear-colors [
      set loser self
      stop
    ]

    ; 4. eat food
    if pcolor = green [
      make-food
      set snake-age snake-age + 1
    ]

    ; 5. check if max age reached (and thus game won)
    if snake-age >= max-snake-age [
      set winner self
      stop
    ]

    ; 6. move snake body parts forward
    ask patches with [pcolor = [snake-color] of myself] [
      set age age + 1
      if age > [snake-age] of myself [
        set age -1
        set pcolor black
      ]
    ]

    ; 7. set the patch colour and age of the snake's head.
    set pcolor snake-color
    set age 0
  ]

  ; A collision has happened: show message and stop the game
  (ifelse loser != nobody [
    user-message (word "Game Over! Team " [team] of loser " lost")
    stop
  ] winner != nobody [
    user-message (word "Game Over! Team " [team] of winner " won!")
    stop
  ])
  tick
end


;;--------------------------------------------

;;;
; Make the turtle face a random unoccupied neighboring patch
;  if all patches are occupied, then any patch will be selected (and the snake lose :( )
to face-random-neighboring-patch ; turtle
  let next-patch one-of neighbors4 with [member? pcolor clear-colors]

  if next-patch = nobody [ ; if none of the neighbours are clear:
    set next-patch one-of neighbors4
  ]
  ; make the snake face towards the patch we want the snake to go to:
  face next-patch
end

to pathfind
  ;; Checks if there are obstacles blocking the current path, or if the path is empty or undefined
  if length filter [p -> not member? [pcolor] of p clear-colors] current-path > 0 or length current-path = 0 or current-path = nobody [
    let memory nobody      ;; Initializes memory variable
    let start patch-here   ;; Sets the start patch to the current patch
    ;; Finds the target patch by sorting patches based on their distance from the start patch
    let target first sort-by [[p1 p2] -> [distance start] of p1 < [distance start] of p2] patches with [pcolor = green]

    ;; Selects the appropriate pathfinding algorithm based on the mode variable
    (ifelse
      mode = "depth" [
           set memory x-first-search start target
       ]
       mode = "breadth" [
           set memory x-first-search start target
       ]
       mode = "uniform" [
           set memory uniform-search start target
       ]
       mode = "greedy" [
           set memory greedy-search start target
       ]
       mode = "a*" [
           set memory a-star-search start target
       ]
      mode = "bi-directional" [
           set memory bi-directional-search start target
      ]
    ; else human controlled -- do nothing
    []
    )

    ;; Checks if a valid path has been found
    ifelse memory != nobody [
      let path []       ;; Initializes the path list
      let node target   ;; Sets the current node to the target
      ;; Traces back the path from the target to the start using the memory table
      while [node != nobody] [
        set path fput node path
        set node table:get-or-default memory pcors node nobody
      ]
      set current-path but-first path   ;; Sets the current path to the traced path
    ] [
      ;; If no valid path is found, randomly selects a neighboring patch as the next move
      let moves get-adjcells start
      if length moves > 0 [
        set current-path (list one-of moves)
      ]
    ]
  ]

  ;; Moves the agent along the current path if it exists
  if length current-path > 0 [
    let next-patch first current-path   ;; Selects the next patch in the current path
    set current-path but-first current-path   ;; Updates the current path without the first patch
    if next-patch != nobody [
      face next-patch   ;; Faces the agent towards the next patch in the path
    ]
    ;;
    ;; Optional: Visualize the current path by changing the color of patches along the path
    if length current-path > 0 and false [
      foreach but-last current-path [n ->
        ask n [set pcolor cyan] ;; Show current path
      ]
    ]
    ;;
  ]
end


;;===============================================
;; Supporting Functions/Procedures
;;===============================================

;; Function to get the adjacent cells of a given patch
to-report get-adjcells [current]
  let [x y] pcors current  ;; Extracts x and y coordinates of the current patch
  let adjcells []          ;; Initializes list to store orthogonal neighbors
  let offsets [[-1 0] [0 -1] [1 0] [0 1]]  ;; Defines offsets for orthogonal movement

  foreach offsets [o ->
    let nx x + first o     ;; Calculates new x-coordinate based on offset
    let ny y + last o      ;; Calculates new y-coordinate based on offset
    let p patch nx ny      ;; Retrieves the patch at the new coordinates
    if p != nobody [       ;; Checks if the patch exists
      if member? [pcolor] of p clear-colors [  ;; Verifies if the patch is walkable
        set adjcells lput p adjcells  ;; Adds the walkable patch to orthogonal neighbors list
      ]
    ]
  ]
  report adjcells  ;; Returns the list of adjacent neighbours
end

;; Function to retrieve x and y coordinates of a patch
to-report pcors [p]
  report (list [pxcor] of p [pycor] of p)  ;; Returns a list containing a patches' [x y] coordinates
end

;; Function to calculate the Manhattan distance between two patches
;; Formatted using the "Manhattan" function
to-report heuristic [p t]
  let [px py] pcors p  ;; Extracts x and y coordinates of the current patch
  let [tx ty] pcors t  ;; Extracts x and y coordinates of the target patch
  report abs (px - tx) + abs (py - ty)  ;; Returns the Manhattan distance between the two patches
end


;;=======================================
;; Breadth & Depth First Search
;; This is represented as "x-first-search" as depth and breadth searches are more or less opposites
;;=======================================

to-report x-first-search [start target]
  let frontier (list start)           ;; Initializes starting node frontier
  let explored []                     ;; Initializes an empty explored set
  let memory (table:make)             ;; Initializes memory table to keep track of the parent-child relationship
  let current nobody                  ;; Initializes the current node

  table:put memory pcors start nobody ;; Set the starting node's parent as "nobody"
  let path-found false
  while [not empty? frontier and not path-found] [ ;; Loops until the frontier is empty or path is found
    ;; Pop from the frontier based on the search mode
    (ifelse mode = "depth" [          ;; Depth-first search (LIFO)
      set current last frontier
      set frontier but-last frontier
    ] mode = "breadth" [              ;; Breadth-first search (FIFO)
      set current first frontier
      set frontier but-first frontier
    ])
    ;; Add current to the explored set
    set explored remove-duplicates lput current explored
    foreach get-adjcells current [neighbor ->
      ;; Create link from child node to its' parent
      if not table:has-key? memory pcors neighbor [table:put memory pcors neighbor current]
      ;; If neighbor is neither in frontier nor explored, add it to the frontier
      if not member? neighbor frontier and not member? neighbor explored [
        ifelse target = neighbor [     ;; If neighbor is the target, set path-found to true
          set path-found true
        ] [
          set frontier lput neighbor frontier ;; Add neighbor to the end of frontier
        ]
      ]
    ]
  ]


  if not path-found
  [
    report nobody
  ]
  report memory
end

;;=======================================
;; Uniform Search
;;=======================================

to-report uniform-search [start target]
  let frontier (list start) ;; Initializes frontier with the starting node
  let explored []           ;; Initializes an empty list to keep track of explored nodes
  let memory (table:make)   ;; Initializes a memory table to store parent-child relationships
  let costs (table:make)    ;; Initializes costs table to store the cost of reaching each node
  let current nobody        ;; Initialize variable for current node

  table:put memory pcors start nobody  ;; Sets parent of the starting node to nobody
  table:put costs pcors start 0
  let path-found false                 ;; Initialize variable to track if path is found

  while [not empty? frontier and not path-found] [  ;; Loops until frontier is empty or path is found
    set current first frontier
    if current = target [   ;; If current node is the target, set path-found to true
      set path-found true
    ]

    set frontier but-first frontier    ;; Removes the current node from the frontier
    set explored remove-duplicates lput current explored    ;; Adds the current node to the "explored" list
    foreach get-adjcells current [neighbor ->               ;; Iterates over neighbours of the current node
      if not member? neighbor explored [
        let cost table:get costs pcors current + 1          ;; Calculates the cost to reach the neighbor
        if cost < table:get-or-default costs pcors neighbor 10000 [    ;; Update cost if the new cost is lower than existing cost
          table:put costs pcors neighbor cost
          table:put memory pcors neighbor current           ;; Updates memory to store paren
        ]
        if not member? neighbor frontier [                  ;; Adds neighbor to frontier if not present, sorted by cost
          set frontier sort-by [[p1 p2] -> table:get costs pcors p1 < table:get costs pcors p2] fput neighbor frontier
        ]
      ]
    ]
  ]
  if not path-found [
    report nobody
  ]
  report memory
end

;;=======================================
;; Greedy Search
;;=======================================

to-report greedy-search [start target]
  let open (list start)
  let closed []
  let memory (table:make)  ;; Initializes memory table, keeping track of previous patch
  let fMap (table:make)    ;; Initializes map to store the estimated total cost from start to target through each node
  let current nobody

  table:put memory pcors start nobody                ;; Mark the starting node in the memory table
  table:put fMap pcors start heuristic start target  ;; Calculate and store heuristic value for the starting node
  let path-found false                               ;; Initialize a flag to indicate if the path has been found

  while [not empty? open and not path-found] [       ;; Loops until open list is empty or path is found
    set current first open                           ;; Retrieves the first node from the open list
    set open but-first open                          ;; Remove the current node from the open list
    set closed remove-duplicates lput current closed ;; Add the current node to the closed list
    foreach get-adjcells current [neighbor ->        ;; Iterate over the neighbors of the current node
      if not table:has-key? memory pcors neighbor [table:put memory pcors neighbor current]   ;; If the neighbor is not in memory table, mark its parent
      if not member? neighbor open and not member? neighbor closed [  ;; If statement for if the neighbor is not in the open list and not in the closed list
        ifelse neighbor = target [                   ;; If the neighbor is the target, set path-found to true
          set path-found true
        ] [
          table:put fMap pcors neighbor heuristic neighbor target     ;; Calculate and store heuristic value for the neighbor
          set open sort-by [[p1 p2] -> table:get fMap pcors p1 < table:get fMap pcors p2] fput neighbor open  ;; Add the neighbor to the open list sorted by heuristic value
        ]
      ]
    ]
  ]

  if not path-found [
    report nobody
  ]
  report memory

end

;;=======================================
;; A* Search
;;=======================================

to-report a-star-search [start target]
  ;; Note - Similar to greedy but more focused on the most optimal side
  let open (list start)
  let closed []
  let memory (table:make)  ;; Initializes memory table, keeping track of previous patch
  let gMap (table:make)    ;; Initializes map to store the cost from start to each node (distance travelled)
  let fMap (table:make)    ;; Initializes map to store the estimated total cost from start to target through each node
  let current nobody

  table:put memory pcors start nobody
  table:put gMap pcors start 0
  table:put fMap pcors start heuristic start target
  let path-found false

  ;; Loops until open list is empty or path is found
  while [not empty? open and not path-found] [
    set current first open
    ;; If current node is the target, set path-found to true
    if current = target [
      set path-found true
    ]
    set open but-first open   ;; Removes current node from open list
    set closed remove-duplicates lput current closed  ;; Add current node to closed list
    foreach get-adjcells current [neighbor ->   ;; Iterate over neighbors of current node
      let tempG table:get gMap pcors current + 1  ;; Calculates temporary cost from start to neighbor
      if tempG < table:get-or-default gMap pcors neighbor 10000 [  ;; If temporary cost is less than the current cost to neighbor
        table:put memory pcors neighbor current  ;; Update memory to store parent of neighbor
        table:put gMap pcors neighbor tempG    ;; Update cost from start to neighbor
        table:put fMap pcors neighbor tempG + heuristic neighbor target ;; Update estimated total cost from start to target through neighbor
        ;; If neighbor is not in open list and not in closed list, add it to open list
        if not member? neighbor open and not member? neighbor closed [
          ;; Add neighbor to open list sorted by estimated total cost
          set open sort-by [[p1 p2] -> table:get fMap pcors p1 < table:get fMap pcors p2] fput neighbor open
        ]
      ]
    ]
  ]

  if not path-found
  [
    report nobody
  ]
  report memory

end

;;=======================================
;; Bi-Directional Search
;;=======================================

to-report bi-directional-search [start target]
  let start-frontier (list start) ;; Initialize frontier for start-to-target search
  let start-explored []           ;; Initialize explored set for start-to-target search
  let start-memory (table:make)   ;; Initialize memory table for start-to-target search
  let start-costs (table:make)    ;; Initialize costs table for start-to-target search
  let start-current nobody        ;; Initialize variable for current node in start-to-target search

  let target-frontier (list target) ;; Initialize frontier for target-to-start search
  let target-explored []            ;; Initialize explored set for target-to-start search
  let target-memory (table:make)    ;; Initialize memory table for target-to-start search
  let target-costs (table:make)     ;; Initialize costs table for target-to-start search
  let target-current nobody         ;; Initialize variable for current node in target-to-start search

  ;; Set up start-to-target search
  table:put start-memory pcors start nobody    ;; Set start as parent of start
  table:put start-costs pcors start 0          ;; Set cost to start as 0
  let start-path-found false                  ;; Initialize flag for path found in start-to-target search

  ;; Set up target-to-start search
  table:put target-memory pcors target nobody    ;; Set target as parent of target
  table:put target-costs pcors target 0          ;; Set cost to target as 0
  let target-path-found false                    ;; Initialize flag for path found in target-to-start search

  while [(not start-path-found or not target-path-found) and not empty? start-frontier and not empty? target-frontier] [
    ;; Start-to-target search
    if not start-path-found [
      set start-current first start-frontier       ;; Get the current node from start-to-target frontier
      set start-frontier but-first start-frontier  ;; Remove the current node from start-to-target frontier
      set start-explored remove-duplicates lput start-current start-explored ;; Add current node to start-to-target explored set
      if start-current = target [ ;; If current node in start-to-target search equals target, set path found to true
        set start-path-found true
      ]
      foreach get-adjcells start-current [neighbor -> ;; Iterate over neighbors of current node in start-to-target search
        if not member? neighbor start-explored [
          let cost table:get start-costs pcors start-current + 1 ;; Calculate cost to reach neighbor
          if cost < table:get-or-default start-costs pcors neighbor 10000 [ ;; Update cost if lower than current cost
            table:put start-costs pcors neighbor cost
            table:put start-memory pcors neighbor start-current ;; Update memory with parent of neighbor
          ]
          if not member? neighbor start-frontier [ ;; Add neighbor to start-to-target frontier if not already present
            set start-frontier lput neighbor start-frontier
          ]
        ]
      ]
    ]

    ;; Target-to-start search
    if not target-path-found [
      set target-current first target-frontier     ;; Get the current node from target-to-start frontier
      set target-frontier but-first target-frontier ;; Remove the current node from target-to-start frontier
      set target-explored remove-duplicates lput target-current target-explored ;; Add current node to target-to-start explored set
      if target-current = start [ ;; If current node in target-to-start search equals start, set path found to true
        set target-path-found true
      ]
      foreach get-adjcells target-current [neighbor -> ;; Iterate over neighbors of current node in target-to-start search
        if not member? neighbor target-explored [
          let cost table:get target-costs pcors target-current + 1 ;; Calculate cost to reach neighbor
          if cost < table:get-or-default target-costs pcors neighbor 10000 [ ;; Update cost if lower than current cost
            table:put target-costs pcors neighbor cost
            table:put target-memory pcors neighbor target-current ;; Update memory with parent of neighbor
          ]
          if not member? neighbor target-frontier [ ;; Add neighbor to target-to-start frontier if not already present
            set target-frontier lput neighbor target-frontier
          ]
        ]
      ]
    ]
  ]

  ;; Combine paths from start-to-target and target-to-start
  let combined-path nobody
  if start-path-found and target-path-found [
    let intersection find-intersection start-explored target-explored ;; Find intersection of paths
    set combined-path reconstruct-path start-memory target-memory intersection ;; Reconstruct combined path
  ]

  if combined-path != nobody [
    report combined-path
  ]
  report nobody
end

to-report find-intersection [list1 list2]
  ;; Find intersection of two lists
  let intersection filter [x -> member? x list2] list1
  report intersection
end

to-report reconstruct-path [start-memory target-memory intersection]
  ;; Reconstruct combined path from start to target using intersection point
  let path1 []
  let node intersection
  while [node != nobody] [
    set path1 fput node path1
    set node table:get-or-default start-memory pcors node nobody
  ]

  let path2 []
  set node table:get-or-default target-memory pcors intersection nobody
  while [node != nobody] [
    set path2 lput node path2
    set node table:get-or-default target-memory pcors node nobody
  ]

  ;; Combine paths from start to intersection and intersection to target
  let combined-path reverse fput item 0 path1 path2
  report combined-path
end

;;--------------------------------------------

;;---------------------
;; Human controlled snakes:
to head-up [selected-team]
  ask snakes with [team = selected-team] [ set heading 0 ]
end
;----
to head-right [selected-team]
  ask snakes with [team = selected-team] [ set heading 90 ]
end
;----
to head-down [selected-team]
  ask snakes with [team = selected-team] [ set heading 180 ]
end
;----
to head-left [selected-team]
  ask snakes with [team = selected-team] [ set heading 270 ]
end
;;---------------------

;;=======================================================

;; for displaying the age within the GUI:
to-report report-snake-age [team-name]
  report [snake-age] of one-of snakes with [team = team-name]
end

;;---------------------
@#$#@#$#@
GRAPHICS-WINDOW
210
10
680
481
-1
-1
14.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
36
37
109
70
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
36
74
99
107
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
477
486
615
531
red-team-mode
red-team-mode
"human" "random" "depth" "breadth" "uniform" "greedy" "a*" "bi-directional"
3

BUTTON
251
537
306
570
up
head-up \"blue\"
NIL
1
T
OBSERVER
NIL
W
NIL
NIL
1

BUTTON
249
601
304
634
down
head-down \"blue\"
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
196
568
251
601
left
head-left \"blue\"
NIL
1
T
OBSERVER
NIL
A
NIL
NIL
1

BUTTON
303
571
358
604
right
head-right \"blue\"
NIL
1
T
OBSERVER
NIL
D
NIL
NIL
1

CHOOSER
194
485
320
530
blue-team-mode
blue-team-mode
"human" "random" "depth" "breadth" "uniform" "greedy" "a*" "bi-directional"
7

BUTTON
537
535
592
568
up
head-up \"red\"
NIL
1
T
OBSERVER
NIL
I
NIL
NIL
1

BUTTON
590
566
645
599
right
head-right \"red\"
NIL
1
T
OBSERVER
NIL
L
NIL
NIL
1

BUTTON
535
599
590
632
down
head-down \"red\"
NIL
1
T
OBSERVER
NIL
K
NIL
NIL
1

BUTTON
481
567
536
600
left
head-left \"red\"
NIL
1
T
OBSERVER
NIL
J
NIL
NIL
1

CHOOSER
34
135
181
180
map-file
map-file
"empty" "snake-map-1" "snake-map-2" "snake-map-3" "blank"
1

SWITCH
33
188
181
221
two-player
two-player
0
1
-1000

TEXTBOX
690
448
912
486
You need to press setup after changing the map or modes.
12
0.0
1

SLIDER
32
230
181
263
max-snake-age
max-snake-age
3
30
10.0
1
1
NIL
HORIZONTAL

MONITOR
324
486
399
531
Blue age
report-snake-age \"blue\"
0
1
11

MONITOR
619
487
690
532
Red age
report-snake-age \"red\"
0
1
11

SWITCH
33
271
182
304
extensions
extensions
0
1
-1000

@#$#@#$#@
If you want access to this please directly message me
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
