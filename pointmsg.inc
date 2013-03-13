;----------------------------------------------------------------------------#
;                                                                            #
; This file is copyrighted by Michael Lee Finney. It is hereby released into #
; the public domain and may be used for any purpose, private or commercial   #
; providing only that credit for the original code is included. The source   #
; code may be modified in any manner.                                        #
;----------------------------------------------------------------------------#

dseg     segment

pntMsg0  db       9,'Point 0',cr,lf
pntMsg1  db       9,'Point 1',cr,lf
pntMsg2  db       9,'Point 2',cr,lf
pntMsg3  db       9,'Point 3',cr,lf
pntMsg4  db       9,'Point 4',cr,lf
pntMsg5  db       9,'Point 5',cr,lf
pntMsg6  db       9,'Point 6',cr,lf
pntMsg7  db       9,'Point 7',cr,lf
pntMsg8  db       9,'Point 8',cr,lf
pntMsg9  db       9,'Point 9',cr,lf
pntMsg10 db       10,'Point 10',cr,lf
pntMsg11 db       10,'Point 11',cr,lf

dseg     ends

cseg     segment

         align    4

Point0 proc near

         pusha
         pushf
         mov      bx,offset pntMsg0
         call     PutString
         popf
         popa
         ret

Point0 endp

         align    4

Point1 proc near

         pusha
         pushf
         mov      bx,offset pntMsg1
         call     PutString
         popf
         popa
         ret

Point1 endp

         align    4

Point2 proc near

         pusha
         pushf
         mov      bx,offset pntMsg2
         call     PutString
         popf
         popa
         ret

Point2 endp

         align    4

Point3 proc near

         pusha
         pushf
         mov      bx,offset pntMsg3
         call     PutString
         popf
         popa
         ret

Point3 endp

         align    4

Point4 proc near

         pusha
         pushf
         mov      bx,offset pntMsg4
         call     PutString
         popf
         popa
         ret

Point4 endp

         align    4

Point5 proc near

         pusha
         pushf
         mov      bx,offset pntMsg5
         call     PutString
         popf
         popa
         ret

Point5 endp

         align    4

Point6 proc near

         pusha
         pushf
         mov      bx,offset pntMsg6
         call     PutString
         popf
         popa
         ret

Point6 endp

         align    4

Point7 proc near

         pusha
         pushf
         mov      bx,offset pntMsg7
         call     PutString
         popf
         popa
         ret

Point7 endp

         align    4

Point8 proc near

         pusha
         pushf
         mov      bx,offset pntMsg8
         call     PutString
         popf
         popa
         ret

Point8 endp

         align    4

Point9 proc near

         pusha
         pushf
         mov      bx,offset pntMsg9
         call     PutString
         popf
         popa
         ret

Point9 endp

         align    4

Point10 proc near

         pusha
         pushf
         mov      bx,offset pntMsg10
         call     PutString
         popf
         popa
         ret

Point10 endp

         align    4

Point11 proc near

         pusha
         pushf
         mov      bx,offset pntMsg11
         call     PutString
         popf
         popa
         ret

Point11 endp

cseg     ends
