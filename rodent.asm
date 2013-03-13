         title    Copyright (c) 1993..2011 by Michael Lee Finney. All rights reserved.

;----------------------------------------------------------------------------#
;                                                                            #
; This file is copyrighted by Michael Lee Finney. It is hereby released into #
; the public domain and may be used for any purpose, private or commercial   #
; providing only that credit for the original code is included. The source   #
; code may be modified in any manner.                                        #
;----------------------------------------------------------------------------#

         .386p

;----------------------------------------------------------------------------#
;                                                                            #
;    This is a device dependent mouse driver.  Many different types of mice  #
; are supported.  It is designed to work in conjunction with MOUSE.SYS       #
; (provided by IBM).                                                         #
;                                                                            #
;----------------------------------------------------------------------------#

dseg     segment  word public use16 'data'
dseg     ends

cseg     segment  word public use16 'code'
         assume   cs:cseg,ss:nothing,es:nothing,ds:dseg

         extrn    DOSBEEP:far
         extrn    DOSCLOSE:far
         extrn    DOSDEVIOCTL:far
         extrn    DOSOPEN:far
         extrn    DOSPUTMESSAGE:far
         extrn    DOSREAD:far
         extrn    DOSWRITE:far

cseg     ends

dgroup   group    dseg

         include  aberror.inc
         include  devsym.inc
         include  devhlp.inc
         include  iodelay.inc
         include  infoseg.inc

;----------------------------------------------------------------------------#
;                                                                            #
;    This section contains all structure definitions.                        #
;                                                                            #
;----------------------------------------------------------------------------#

eventRelative  equ      0001h          ; Command to process relative mouse packet
eventAbsolute  equ      0003h          ; Command to process absolute mouse packet

AttachDd struc

   realEntry   dd       ?              ; Real mode entry point
   realDS      dw       ?              ; Real mode data segment
   protEntry   dd       ?              ; Protect mode entry point
   protDS      dw       ?              ; Protect mode data selector

AttachDd ends

RelativeEvent struc

   event       dw       ?              ; Mouse event
   colMov      dw       ?              ; Column movement
   rowMov      dw       ?              ; Row movement

RelativeEvent ends

AbsoluteEvent struc

   eventRename dw       ?              ; Mouse event (duplicate name, use "event")
   rowPos      dw       ?              ; Row position relative to 0
   colPos      dw       ?              ; Column position relative to 0
   rowSize     dw       ?              ; Number of row units (0 based?)
   colSize     dw       ?              ; Number of column units (0 based?)

AbsoluteEvent ends

DevData struc

   cfgDataLen  dw       ?              ; Length of data (9)
   numMics     db       ?              ; Number mickeys/cm
   numButt     db       ?              ; Number of buttons
   irq         db       ?              ; Irq level
   mouseType   db       ?              ; Mouse type attached
   comPortNum  db       ?              ; Communication port number
   portAddr    dw       ?              ; Communication port address

DevData ends

PortState struc

   ratState    dw       ?              ; Baud rate
   divState    dw       ?              ; Divisor latch state
   lcrState    db       ?              ; Line control register state
   mcrState    db       ?              ; Modem control register state
   ierState    db       ?              ; Interrupt control register state
   fcrState    db       ?              ; FIFO control register state
   scrState    db       ?              ; Scratch register state
   afrState    db       ?              ; Alternate function register state

PortState ends

;----------------------------------------------------------------------------#
;                                                                            #
;    OS/2 Device Driver Header -- this MUST be first in the data segment.    #
;                                                                            #
;----------------------------------------------------------------------------#

dseg     segment

Header   label    word
         dw       -1             ; Device header pointer
         dw       -1             ; Device header pointer
         dw       0c180h         ; Device Driver attributes
         dw       CmdEntry       ; Strategy Routine Offset
         dw       IdcEntry       ; Entry point for IDC
         db       'RODENT$ '     ; Default device Name
         dw       0              ; Protect-mode CS Strategy Selector
         dw       0              ; Protect-mode DS selector
         dw       0              ; Real-mode CS Strategy Segment
         dw       0              ; Real-mode DS segment
         dd       00000012h      ; Can handle > 16MB physical memory
                                 ;  and accepts initComplete packets

dseg     ends

;----------------------------------------------------------------------------#
;                                                                            #
;    This section contains all general data elements and equates.            #
;                                                                            #
;----------------------------------------------------------------------------#

dseg     segment

;
;------------------------ Strategy Packet Definitions ------------------------
;

unknownCmd     equ      8003h          ; Unknown command OS/2 error code
initBad        equ      802fh          ; Fatal init error (abort DD load)

;
;----------------------- Control Character Definitions -----------------------
;

soh            equ      01h            ; ASCII code for start of header
stx            equ      02h            ; ASCII code for start of tramsmit
etx            equ      03h            ; ASCII code for end of transmission
tab            equ      09h            ; ASCII code for tab
lf             equ      0ah            ; ASCII code for line feed
cr             equ      0dh            ; ASCII code for carriage return
xon            equ      11h            ; ASCII code for X-ON
xoff           equ      13h            ; ASCII code for X-OFF
escape         equ      1bh            ; ASCII code for escape

;
;------------------------- Device Status Definitions -------------------------
;

readEnable     equ      0001h          ; 00000001 Packet reporting enabled
attached       equ      0002h          ; 00000010 MOUSE$ attach done
irqEnabled     equ      0004h          ; 00000100 Irq enabled
deinstalled    equ      0008h          ; 00001000 Driver has been deinstalled
deviceEnabled  equ      0010h          ; 00010000 Device enabled

devStatus      dw       0              ; Current device status

;
;-------------------------- Device Data Definitions --------------------------
;

cfgDatLen      equ      9              ; Size of configuration data

busMouse       equ      1              ; Buss mouse
serMouse       equ      2              ; Serial mouse
inpMouse       equ      3              ; Inport mouse
ps2Mouse       equ      4              ; PS/2 mouse
pscMouse       equ      5              ; Prescott touch display
logMouse       equ      6              ; Logitech mouse
kypMouse       equ      7              ; Screen Reader Keypad

deviceData     DevData  <cfgDatLen,0,3,0,serMouse,0,0>

;
;------------------ Inter-device Communciation Definitions -------------------
;

globalInfoSeg  equ      1              ; Global info segment variable number
localInfoAddr  equ      2              ; Local info address

globalInfo     dd       0              ; Pointer to global information structure
localInfoPtr   dd       0              ; Address of pointer to local information structure

deviceHelp     dd       0              ; Device helper entry point

intPacketOff   dw       0              ; Offset in Mouse DS for int packets

mseDd          AttachDd <>             ; Mse DD Interface Data Record

mseName        db       'MOUSE$  ',0   ; Name of device independent driver


;
;----------------------------- ABIOS Definitions -----------------------------
;

isAbios        db       0              ; Flag to indicate ABIOS is present
gotLid         db       0              ; Flag to indicate got LID from ABIOS
devLid         dw       0              ; Serial port allocated from ABIOS

;
;--------------------- Interrupt Controller Definitions ----------------------
;

mstr8259       equ      021h           ; Master 8259 mask register port
slav8259       equ      0a1h           ; Slave 8259 mask register port

irr8259        equ      0ah            ; Select 8259 interrupt request reg
isr8259        equ      0bh            ; Select 8259 interrupt in service reg

mskPrt8259     dw       0              ; Mask register port for 8259
disable8259    db       ?              ; Disable IRQ level mask for the 8259
disableKeybrd  equ      02h            ; Disable IRQ level mask for keyboard
enable8259     db       ?              ; Enable IRQ level mask for the 8259
enableKeybrd   equ      0fdh           ; Enable IRQ level mask for keyboard

;
;-------------------------- Buss Mouse Definitions ---------------------------
;
; Note: The bus mouse is based on an 8255A, we have to program the 8255A
;       properly in order to function. We need to select:
;
;    Mode 0  for all ports
;
;    Port A  input - this allows us to read information from the ALPS
;            chip and from the buttons
;
;    Port B  output - this allows us to write a value there and then
;            read it back in. It is not connected to anything, but can
;            be used to check for the presence of the mouse.
;
;    Port C, upper nibble output - used to send control info to the ALPS chip
;
;    Port C, lower nibble input  - used to poll the state of irq's 2..5
;
; The value that does this is:
;
;       10010001
;       ||||||||-- Port C Lower nibble is input
;       |||||||--- Port B is output
;       ||||||---- Mode 0 for Group B (Port B and lower nibble of C)
;       |||||----- Port C Upper nibble is output
;       ||||------ Port A is input
;       |--------- Mode 0 for Group A (Port A and upper nibble of C)
;       ---------- Mode set control word
;

busBase  equ      023ch       ; Base I/O address of buss mouse
busData  equ      0           ; Mouse data prt
busSig   equ      1           ; Mouse signal port
busCtrl  equ      2           ; Mouse control port
busInit  equ      3           ; Mouse initialize port

busSetup equ      10010001b   ; Value used to initialize buss adapter

irqEnbl  equ      080h        ; Request to enable buss interrupts
irqDsbl  equ      090h        ; Request to disable buss interrupts

lowX     equ      090h        ; Request low nibble of deltaX
hiX      equ      0b0h        ; Request high nibble of deltaX
lowY     equ      0d0h        ; Request low nibble of deltaY
hiY      equ      0f0h        ; Request high nibble of deltaY

;
;----------------------- Inport Buss Mouse Definitions -----------------------
;

inpPrim  equ      023ch       ; Primary base i/o address of Inport mouse
inpSec   equ      0238h       ; Secondary base i/o address of Inport mouse

inpAddr  equ      0           ; Address register offset
inpData  equ      1           ; Data register offset
inpId    equ      2           ; Id register offset
inpTest  equ      3           ; Test register offset

inpStat  equ      0           ; Status register number
inpData1 equ      1           ; Data register number 1
inpData2 equ      2           ; Data register number 2
inpMode  equ      7           ; Mode register number

inPortId equ      0deh        ; Inport type code in ID register

inpReset equ      10000000b   ; Command to reset Inport device

inpHz0I0 equ      00000000b   ; Commands to select reporting rate,
inpHz30  equ      00000001b   ;  0, 30, 50, 100 and 200 allowed.  Zero
inpHz50  equ      00000010b   ;  may have interrupts enabled or disabled
inpHz100 equ      00000011b   ;
inpHz200 equ      00000100b   ;
inpHz0I1 equ      00000110b   ;

inpDataE equ      00001000b   ; Command to enable data interrupt
inpTimrE equ      00010000b   ; Command to enable timer interrupt
inpHoldE equ      00100000b   ; Command to enable hold
inpHoldD equ      11011111b   ; Command to disable hold

inpMovmt equ      01000000b   ; Bit indicating movement
inpBttns equ      00000111b   ; Buttons 1, 2 and 3

inpMastr equ      10111100b   ; IRQs 7, 5, 4, 3 and 2
inpSlave equ      10011110b   ; IRQs 15, 12, 11, 10 and 9

;
;------------------------- Serial Mouse Definitions --------------------------
;
;                             ;----------------------------------------------
;    Port address offsets     ;   W
; for serial port control     ; R r D
; registers.                  ; e i l
;                             ; a t a
;                             ; d e b
                              ;----------------------------------------------
divLsb   equ      0           ; x x 1 Divisor latch (least significant byte)
divMsb   equ      1           ; x x 1 Divisor latch (most significant byte)
afr      equ      2           ; x x 1 Alternate function register (16552 only)
                              ;----------------------------------------------
rxb      equ      0           ; x   0 Receive buffer register
txb      equ      0           ;   x 0 Transmit buffer register
ier      equ      1           ; x x 0 Interrupt enable register
iir      equ      2           ; x   0 Interrupt identification register
fcr      equ      2           ;   x 0 FIFO control register
lcr      equ      3           ; x x   Line control register
mcr      equ      4           ; x x   Modem control register
lsr      equ      5           ; x     Line status register
msr      equ      6           ; x x   Modem status register
scr      equ      7           ; x x   Scratch register
                              ;----------------------------------------------

;
;    Divisor latch values for the various baud rates.  The values are
; obtained by dividing the clock rate (1.8432 MHz) by 16x the desired
; baud rate.
;

div150   equ      0300h
div300   equ      0180h
div600   equ      00c0h
div1200  equ      0060h
div2400  equ      0030h
div4800  equ      0018h
div9600  equ      000ch
div19200 equ      0006h

;
; Interrupt register enable flags
;

ierRx    equ      00000001b      ; Received data available
ierTx    equ      00000010b      ; Transmit buffer empty
ierLx    equ      00000100b      ; Line status change
ierMx    equ      00001000b      ; Modem status change

;
; Interrupt identification register flags
;

iirFifo  equ      11000000b      ; FIFO Registers enabled mask
iirId    equ      00001110b      ; Interrupt id mask
iirPend  equ      00000001b      ; Interrupt pending mask (0 = pending)

;
; FIFO Control register flags
;

fcrTgr01 equ      00000000b      ; Trigger level at 1 byte
fcrTgr04 equ      01000000b      ; Trigger level at 4 bytes
fcrTgr08 equ      10000000b      ; Trigger level at 8 bytes
fcrTgr14 equ      11000000b      ; Trigger level at 14 bytes
fcrTMask equ      11000000b      ; Trigger mask
fcrXmit  equ      00000100b      ; FIFO Transmitter buffer clear
fcrRcvr  equ      00000010b      ; FIFO Receiver buffer clear
fcrBfrd  equ      00000001b      ; FIFO Buffers enabled
fcrOff   equ      00000000b      ; FIFO Buffers disabled

;
; Line control register flags
;

lcrDlab  equ      10000000b      ; Divisor latch access bit (DLAB)
lcrBreak equ      01000000b      ; Transmit break

lcrPnone equ      00000000b      ; None parity
lcrPodd  equ      00001000b      ; Odd parity
lcrPeven equ      00011000b      ; Even parity
lcrPmark equ      00101000b      ; Mark parity
lcrPspac equ      00111000b      ; Space parity
lcrPmask equ      00111000b      ; Parity mask

lcrStop1 equ      00000000b      ; 1 Stop bit
lcrStop2 equ      00000100b      ; 2 Stop bits (1.5 if 5 data bits)
lcrSmask equ      00000100b      ; Stop bits mask

lcrBits5 equ      00000000b      ; 5 Data bits
lcrBits6 equ      00000001b      ; 6 Data bits
lcrBits7 equ      00000010b      ; 7 Data bits
lcrBits8 equ      00000011b      ; 8 Data bits
lcrBmask equ      00000011b      ; Data bits mask

lcrMask  equ      01111111b      ; Line control register mask

;
; Modem control register flags
;

mcrLoop  equ      00010000b      ; Loopback mode
mcrOut2  equ      00001000b      ; Output 2 (auxilary interrupt enable)
mcrOut1  equ      00000100b      ; Output 1
mcrRts   equ      00000010b      ; Request to send active
mcrDtr   equ      00000001b      ; Data terminal ready active

;
; Line status register flags
;

lsrFerr  equ      10000000b      ; Error in receiver FIFO register
lsrXmtS  equ      01000000b      ; Transmitter shift register empty
lsrXmtH  equ      00100000b      ; Transmitter holding register empty
lsrBreak equ      00010000b      ; Break interrupt
lsrFrame equ      00001000b      ; Framing error
lsrPrty  equ      00000100b      ; Parity error
lsrOvrn  equ      00000010b      ; Overrun error
lsrData  equ      00000001b      ; Data ready

;
; Modem status register flags
;

msrDcd   equ      10000000b      ; Data carrier detect
msrRing  equ      01000000b      ; Ring indicator
msrDsr   equ      00100000b      ; Data set ready
msrCts   equ      00010000b      ; Clear to send
msrDdcd  equ      00001000b      ; Delta data carrier detect
msrTeri  equ      00000100b      ; Trailing edge ring indicator
msrDdsr  equ      00000010b      ; Delta data set ready
msrDcts  equ      00000001b      ; Delta clear to send

ins8250  equ      1              ; INS8250, INS8250-B
ns16450  equ      2              ; INS8250A, INS82C50A, NS16450, NS16C450
ns16550  equ      3              ; NS16550
ns16550a equ      4              ; NS16550A
ns16c552 equ      5              ; NS16C552

uartType db       0              ; Type of uart

;
; Serial port state.
;

portSave PortState   <0,0,0,0,0,0,0,0>

;
;-------------------------- PS/2 Mouse Definitions ---------------------------
;

ps2Data  equ      60h            ; Keyboard/auxilary data port (i/o)
ps2Ctrl  equ      64h            ; Keyboard/auxilary command port (output)
ps2Stat  equ      64h            ; Keyboard/auxilary status port (input)
ps2KyIrq equ      1              ; Keyboard interrupt
ps2AxIrq equ      12             ; Auxilary interrupt

;
; 8042 Status byte
;

ps2Perr  equ      10000000b      ; Parity error
ps2Time  equ      01000000b      ; Time out
ps2AxFl  equ      00100000b      ; Auxilary device output buffer full
ps2Inhbt equ      00010000b      ; Keyboard inhibited switch
ps2Cmd   equ      00001000b      ; Command (set) / Data (cleared) flag
ps2Systm equ      00000100b      ; System flag
ps2Inpt  equ      00000010b      ; Controller input buffer full
ps2Otpt  equ      00000001b      ; Controller output buffer full

;
; 8042 Controller command byte (at controller RAM address 0)
;

ps2Rsrd1 equ      10000000b      ; Reserved (always 0)
ps2Xlate equ      01000000b      ; IBM Keyboard translation mode
ps2AuxDs equ      00100000b      ; Disable auxilary device
ps2KeyDs equ      00010000b      ; Disable keyboard device
ps2Rsvd2 equ      00001000b      ; Reserved (always 0)
ps2SysFg equ      00000100b      ; System flag
ps2AuxEn equ      00000010b      ; Enable auxilary interrupt (irq 12)
ps2KeyEn equ      00000001b      ; Enable keyboard interrupt (irq 1)

;
; 8042 Commands
;

ps2CmdRd equ      020h           ; Read command byte  (20h + RAM address)
ps2CmdWr equ      060h           ; Write command byte (60h + RAM address)
ps2PwdTs equ      0a4h           ; Test if password is installed
ps2PwdLd equ      0a5h           ; Load password
ps2PwdEn equ      0a6h           ; Enable password (requires loaded password)
ps2DsAux equ      0a7h           ; Disable auxiliary device
ps2EnAux equ      0a8h           ; Enable auxiliary device
ps2TsAux equ      0a9h           ; Test auxiliary interface
ps2SlfTs equ      0aah           ; Perform internal diagnostics
ps2TsKey equ      0abh           ; Perform keyboard interface test
ps2CmdRs equ      0ach           ; Reserved
ps2DsKey equ      0adh           ; Disable keyboard device
ps2EnKey equ      0aeh           ; Enable keyboard device
ps2RdInp equ      0c0h           ; Read input port and place in output port
ps2PlInL equ      0c1h           ; Poll input port low
ps2PlInH equ      0c2h           ; Poll input port high
ps2RdOut equ      0d0h           ; Read output port
ps2WrOut equ      0d1h           ; Write output port
ps2KeyWr equ      0d2h           ; Write keyboard device output buffer
ps2AuxWr equ      0d3h           ; Write auxilary device output buffer
ps2WrAux equ      0d4h           ; Send data to auxiliary device
ps2RdTst equ      0e0h           ; Read test inputs
ps2Pulse equ      0f0h           ; Pulse output port (0f0h + port bits)

;
; PIX mouse Commands/Responses
;

pixReset equ      0ffh           ; Reset mouse
pixResnd equ      0feh           ; Resend last data stream
pixDgErr equ      0fch           ; Error on mouse diagnostics
pixAck   equ      0fah           ; Acknowledge
pixDeflt equ      0f6h           ; Set default status
pixDisbl equ      0f5h           ; Disable mouse
pixEnabl equ      0f4h           ; Enable mouse
pixRate  equ      0f3h           ; Set sampling rate
pixRdTyp equ      0f2h           ; Read device type
pixRemot equ      0f0h           ; Set prompt mode
pixEcho  equ      0eeh           ; Set echo mode
pixKlEch equ      0ech           ; Reset echo mode
pixRdPkt equ      0ebh           ; Prompt mouse for data packet
pixStrm  equ      0eah           ; Set incremental stream mode
pixStat  equ      0e9h           ; Status request
pixResln equ      0e8h           ; Set resolution
pixScl21 equ      0e7h           ; Set scaling 2:1
pixScl11 equ      0e6h           ; Set scaling 1:1
pixDgOk  equ      0aah           ; Diagnostics ok
pixPower equ      0aah           ; Response following power on or reset
pixMsRam equ      05bh           ; read ram byte                (MS only)
pixMsDg  equ      059h           ; enter MS diagnostices mode   (MS only)
pixRdInp equ      056h           ; Read raw Inport data         (MS only)
pixVers  equ      052h           ; Read firmware version number (MS only)
pixDgId  equ      000h           ; Diagnostic ID number

;
; Auxilary device identifiers
;

auxMouse equ      00h            ; Id returned from PIX mouse
auxBlPnt equ      02h            ; Id returned from Ballpoint

;
;------------------------- General Mouse Definitions -------------------------
;

typeAC   equ       1             ; AceCat digitizer
typeBP   equ       2             ; Bit Pad digitizer in Bit Pad One/Two mode
typeC    equ       3             ; Type C mouse, Logitech MS-type 3-button
typeCR   equ       4             ; Bit Pad digitizer in CR mode
typeD3   equ       5             ; CalComp 3300 & newer digitizers
typeD9   equ       6             ; CalComp 9x00 digitizers
typeDW   equ       7             ; CalComp WIZ/2x00 digitizers
typeFX   equ       8             ; Felix mouse
typeKR   equ       9             ; Kurta digitizer -- MM mode
typeM    equ      10             ; Type M mouse, Microsoft 2-button
typeMS   equ      11             ; Type MS mouse, Mouse System's 3-button
typeMW   equ      12             ; Type MW mouse, Logitech 2-button
typeSG   equ      13             ; SummaGraphic digitizer -- master reset supported
typeSM   equ      14             ; SummaGraphic digitizer -- MM mode (4 button puck or stylus)
typeSU   equ      15             ; SummaGraphic digitizer -- UIOF mode (16 button puck)
typeSX   equ      16             ; SummaGraphic digitizer -- FX series
typeUN   equ      17             ; MicroTouch UnMouse
typeV    equ      18             ; Type V mouse, Logitech 3-button
typeVW   equ      19             ; Type VW mouse, Logitech 3-button
typeS    equ      20             ; Type S mouse, any serial mouse
typeB    equ      21             ; Type B mouse, Buss mouse
typeI    equ      22             ; Type I mouse, Inport buss mouse
typeNM   equ      23             ; Type "no mouse" -- dummy mouse handler
typePS   equ      24             ; Type PS mouse, PS/2 port
typeX    equ      25             ; Type X mouse, unknown mouse
typeErr  equ      99             ; Error in validating specified mouse type

mouse    db        0             ; Type of mouse

proto3B  equ       1             ; Mouse System's 3 byte binary protocol
proto5B  equ       2             ; Mouse System's 5 byte binary protocol
protoBA  equ       3             ; SummaGraphic's Bit Pad protocol (absolute)
protoBR  equ       4             ; SummaGraphic's Bit Pad protocol (relative)
protoBS  equ       5             ; Buss mouse protocol
protoCA  equ       6             ; SummaGraphic's CR protocol (absolute)
protoCR  equ       7             ; SummaGraphic's CR protocol (relative)
protoFA  equ       8             ; Felix mouse protocol (absolute)
protoFR  equ       9             ; Felix mouse protocol (relative)
protoHA  equ      10             ; CalComp's high res bin protocol (absolute)
protoHR  equ      11             ; CalComp's high res bin protocol (relative)
protoIN  equ      12             ; Inport Buss mouse protocol
protoMA  equ      13             ; SummaGraphic's MM protocol (absolute)
protoMI  equ      14             ; Microsoft protocol
protoMM  equ      15             ; MM Series protocol
protoMP  equ      16             ; Extension to Microsoft's protocol
protoMR  equ      17             ; SummaGraphic's MM protocol (relative)
protoNM  equ      18             ; Provides "No mouse" support
protoPS  equ      19             ; PS/2 mouse protocol (IBM and Logitech)
protoRE  equ      20             ; Relative Bit Pad One protocol
protoTA  equ      21             ; MicroTouch UnMouse protocol (absolute)
protoTR  equ      22             ; MicroTouch UnMouse protocol (relative)
protoUA  equ      23             ; SummaGraphic's UIOF protocol (absolute)
protoUR  equ      24             ; SummaGraphic's UIOF protocol (relative)

protocol db       0              ; Mouse communication protocol

autoMag  db       0              ; Non-zero if mouse automagically recognized
statsChk db       0              ; Non-zero if mouse can handle *! status command
buttons  db       0              ; Number of buttons on mouse
battLow  db       0              ; Non-zero if battery is low
noise    dw       0ffffh         ; Noise parameter (mouse dependent range)
filter   dw       0ffffh         ; Filter parameter (mouse dependent range)
pressure dw       0ffffh         ; Pressure parameter (mouse dependent range)

rate10   equ      000000001b     ; Mouse can report at  10Hz
rate20   equ      000000010b     ; Mouse can report at  20Hz
rate30   equ      000000100b     ; Mouse can report at  30Hz
rate40   equ      000001000b     ; Mouse can report at  40Hz
rate50   equ      000010000b     ; Mouse can report at  50Hz
rate60   equ      000100000b     ; Mouse can report at  60Hz
rate80   equ      001000000b     ; Mouse can report at  80Hz
rate100  equ      010000000b     ; Mouse can report at 100Hz
rate200  equ      100000000b     ; Mouse can report at 200Hz

mseRate  dw       0              ; Mouse supported reporting rates

mse150   equ      00000001b      ; Mouse can handle   150 baud
mse300   equ      00000010b      ; Mouse can handle   300 baud
mse600   equ      00000100b      ; Mouse can handle   600 baud
mse1200  equ      00001000b      ; Mouse can handle  1200 baud
mse2400  equ      00010000b      ; Mouse can handle  2400 baud
mse4800  equ      00100000b      ; Mouse can handle  4800 baud
mse9600  equ      01000000b      ; Mouse can handle  9600 baud
mse19200 equ      10000000b      ; Mouse can handle 19200 baud

mseSpeed db       0              ; Mouse supported baud rates

sync3Bit equ      04h            ;
sync4Bit equ      08h            ; Bit used to flag 1st packet byte
sync7Bit equ      40h            ;
sync8Bit equ      80h            ;

sizPkt3B equ      3              ;
sizPkt5B equ      5              ;
sizPktBA equ      5              ;
sizPktBR equ      5              ;
sizPktCA equ      5              ;
sizPktCR equ      5              ;
sizPktFA equ      5              ;
sizPktFR equ      5              ;
sizPktHA equ      6              ; Number of bytes in mouse packets
sizPktHR equ      6              ;
sizPktMA equ      5              ;
sizPktMI equ      3              ;
sizPktMM equ      3              ;
sizPktMP equ      4              ;
sizPktMR equ      5              ;
sizPktPS equ      3              ;
sizPktRE equ      5              ;
sizPktTA equ      5              ;
sizPktTR equ      5              ;
sizPktUA equ      8              ;
sizPktUR equ      8              ;

mEvent   db       8 dup(0)       ; Buffer used to build mouse packet
byteCnt  dw       0              ; Packet counter

colRvFlg dw       0              ; X reverse flag (false = 0, true = -1)
colRvVal dw       0              ; X reverse value (N + 1, for N - X)
colMargn dw       0              ; X margin area
colMax   dw       0              ; Maximum absolute X position + 1
colMin   dw       0              ; Minimum absolute X position
colRange dw       0              ; colMax - colMin
colBase  dw       0              ; X offset for absolute window
colBmax  dw       0              ; X maximum value of absolute window base

rowRvFlg dw       0              ; Y reverse flag (false = 0, true = -1)
rowRvVal dw       0              ; Y reverse value (N + 1, for N - X)
rowMargn dw       0              ; Y margin area
rowMax   dw       0              ; Maximum absolute Y position + 1
rowMin   dw       0              ; Minimum absolute Y position
rowRange dw       0              ; rowMax - rowMin
rowBase  dw       0              ; Y offset for absolute window
rowBmax  dw       0              ; Y maximum value of absolute window base

flipFlag dw       0              ; Flag to flip X and Y (false = 0, true = 16)
rounding dw       0              ; Mouse response negative rounding constant
reshift  db       0              ; Mouse response divisor
response db       0              ; Mouse response option (-3..+3)
buffered db       0              ; FIFO Buffering
baudRate dw       0              ; Baud rate
rprtRate dw       0              ; Reporting rate (in Hz)
pComAddr dw       0              ; Pointer offset of com port addr
shared   db       0              ; Flag to share irq
biosData equ      0040h          ; Rom bios data segment address
biosEqpt equ      0010h          ; Rom bios equipment byte
biosMous equ      00000100b      ; Rom bios found installed mouse

;
;------------------------ Button Mapping Definitions -------------------------
;
;    These tables define a mapping from the bit definitions that specify the
; button status in the various protocols to a common internal form which has
; the buttons defined as...
;
;    .... ...1 Button 1 pressed Left button on a mouse
;    .... ..1. Button 2 pressed Middle button on a mouse
;    .... .1.. Button 3 pressed Right button on a mouse
;    .... 1... Button 4 pressed
;
;    ...1 .... Button 5 pressed
;    ..1. .... Button 6 pressed
;    .1.. .... Button 7 pressed
;    0... .... Reserved -- always zero, all other buttons will be ignored
;

btnMapX  equ      $

         db       1
         db       00000000b            ; No buttons pressed

btnMapMI equ      $
btnMapTA equ      $
btnMapTR equ      $

         db       4                    ; LR
         db       00000000b            ; 00 - No buttons pressed
         db       00000100b            ; 01 - R
         db       00000001b            ; 10 - L
         db       00000101b            ; 11 - L+R

btnMapFA equ      $
btnMapFR equ      $

         db       8                    ; RLM
         db       00000000b            ; 000 - No buttons pressed
         db       00000010b            ; 001
         db       00000100b            ; 010 -
         db       00000110b            ; 011 -
         db       00000001b            ; 100 -
         db       00000011b            ; 101 -
         db       00000101b            ; 110 -
         db       00000111b            ; 111 -

btnMap3B equ      $
btnMap5B equ      $
btnMapBS equ      $

         db       8                    ; LMR
         db       00000111b            ; 000 - L+M+R
         db       00000011b            ; 001 - L+M
         db       00000101b            ; 010 - L+R
         db       00000001b            ; 011 - L
         db       00000110b            ; 100 - M+R
         db       00000010b            ; 101 - M
         db       00000100b            ; 110 - R
         db       00000000b            ; 111 - No buttons pressed

btnMapIN equ      $
btnMapMM equ      $
btnMapRE equ      $

         db       8                    ; LMR
         db       00000000b            ; 000 - No buttons pressed
         db       00000100b            ; 001 - R
         db       00000010b            ; 010 - M
         db       00000110b            ; 011 - M+R
         db       00000001b            ; 100 - L
         db       00000101b            ; 101 - L+R
         db       00000011b            ; 110 - L+M
         db       00000111b            ; 111 - L+M+R

btnMapPS equ      $

         db       8                    ; MRL
         db       00000000b            ; 000 - No buttons pressed
         db       00000001b            ; 001 - L
         db       00000100b            ; 010 - R
         db       00000101b            ; 011 - L+R
         db       00000010b            ; 100 - M
         db       00000011b            ; 101 - L+M
         db       00000110b            ; 110 - M+R
         db       00000111b            ; 111 - L+M+R

btnMapMP equ      $

         db       8                    ; MLR
         db       00000000b            ; 000 - No buttons pressed
         db       00000100b            ; 001 - R
         db       00000001b            ; 010 - L
         db       00000101b            ; 011 - L+R
         db       00000010b            ; 100 - M
         db       00000110b            ; 101 - M+R
         db       00000011b            ; 110 - L+M
         db       00000111b            ; 111 - L+M+R

btnMapMA equ      $
btnMapMR equ      $

         db       8                    ; RLM
         db       00000000b            ; 000 - No buttons pressed
         db       00000010b            ; 001 -
         db       00000001b            ; 010 -
         db       00000011b            ; 011 -
         db       00000100b            ; 100 -
         db       00000110b            ; 101 -
         db       00000101b            ; 110 -
         db       00000111b            ; 111 -

btnMapBA equ      $
btnMapBR equ      $

         db       16                   ; FFFF
         db       00000000b            ; 0000 No buttons pressed
         db       00000001b            ; 0001 Button 0
         db       00000010b            ; 0010 Button 1
         db       00000011b            ; 0011 Button   (0+1)
         db       00000100b            ; 0100 Button 2
         db       00000101b            ; 0101 Button   (0+2)
         db       00000110b            ; 0110 Button   (1+2)
         db       00000111b            ; 0111 Button   (0+1+2)
         db       00001000b            ; 1000 Button 3
         db       00001001b            ; 1001 Button   (0+3)
         db       00001010b            ; 1010 Button   (1+3)
         db       00001011b            ; 1011 Button   (0+1+3)
         db       00001100b            ; 1100 Button   (2+3)
         db       00001101b            ; 1101 Button   (0+2+3)
         db       00001110b            ; 1110 Button   (1+2+3)
         db       00001111b            ; 1111 Button   (0+1+2+3)

btnMapCA equ      $
btnMapCR equ      $

         db       8                    ; FFF
         db       00000000b            ; 000 No buttons pressed
         db       00000001b            ; 001 Button 0
         db       00000010b            ; 010 Button 1
         db       00000100b            ; 011 Button 2
         db       00001000b            ; 100 Button 3
         db       00010000b            ; 101 Button 4
         db       00100000b            ; 110 Button 5
         db       01000000b            ; 111 Button 6

btnMapHA equ      $
btnMapHR equ      $

         db       32                   ; CCCCC
         db       00000000b            ; 00000 No buttons pressed
         db       00000001b            ; 00001 Button 0
         db       00000010b            ; 00010 Button 1
         db       00000011b            ; 00011 Button   (0+1)
         db       00000100b            ; 00100 Button 2
         db       00000101b            ; 00101 Button 4 (0+2)
         db       00000110b            ; 00110 Button 5 (1+2)
         db       00000111b            ; 00111 Button   (0+1+2)
         db       00001000b            ; 01000 Button 3
         db       00001001b            ; 01001 Button   (0+3)
         db       00001010b            ; 01010 Button   (1+3)
         db       00001011b            ; 01011 Button   (0+1+3)
         db       00001100b            ; 01100 Button   (2+3)
         db       00001101b            ; 01101 Button   (0+2+3)
         db       00001110b            ; 01110 Button   (1+2+3)
         db       00001111b            ; 01111 Button   (0+1+2+3)
         db       00000001b            ; 10000 Button  0
         db       00000010b            ; 10001 Button  1
         db       00000100b            ; 10010 Button  2
         db       00001000b            ; 10011 Button  3
         db       00010000b            ; 10100 Button  4
         db       00100000b            ; 10101 Button  5
         db       01000000b            ; 10110 Button  6
         db       00000000b            ; 10111 Button  7
         db       00000000b            ; 11000 Button  8
         db       00000000b            ; 11001 Button  9
         db       00000000b            ; 11010 Button 10
         db       00000000b            ; 11011 Button 11
         db       00000000b            ; 11100 Button 12
         db       00000000b            ; 11101 Button 13
         db       00000000b            ; 11110 Button 14
         db       00000000b            ; 11111 Button 15

btnMapUA equ      $
btnMapUR equ      $

         db       32                   ; CCCCC
         db       00000000b            ; 00000 No buttons pressed
         db       00000001b            ; 00001 Button 1
         db       00000010b            ; 00010 Button 2
         db       00000100b            ; 00011 Button 3
         db       00001000b            ; 00100 Button C
         db       00010000b            ; 00101 Button 4
         db       00100000b            ; 00110 Button 5
         db       01000000b            ; 00111 Button 6
         db       00000000b            ; 01000 Button D
         db       00000000b            ; 01001 Button 7
         db       00000000b            ; 01010 Button 8
         db       00000000b            ; 01011 Button 9
         db       00000000b            ; 01100 Button E
         db       00000000b            ; 01101 Button A
         db       00000000b            ; 01110 Button 0
         db       00000000b            ; 01111 Button B
         db       00000000b            ; 10000 Button F
         db       00000000b            ; 10001 Undefined
         db       00000000b            ; 10010 Undefined
         db       00000000b            ; 10011 Undefined
         db       00000000b            ; 10100 Undefined
         db       00000000b            ; 10101 Undefined
         db       00000000b            ; 10110 Undefined
         db       00000000b            ; 10111 Undefined
         db       00000000b            ; 11000 Undefined
         db       00000000b            ; 11001 Undefined
         db       00000000b            ; 11010 Undefined
         db       00000000b            ; 11011 Undefined
         db       00000000b            ; 11100 Undefined
         db       00000000b            ; 11101 Undefined
         db       00000000b            ; 11110 Undefined
         db       00000000b            ; 11111 Undefined

;
;---------------------------- Two Button Mappings ----------------------------
;
;   This table contains the button mappings for two button mice.  The
; general button assignment strategy for two button mice is...
;
;    Button 1 -- report as OS/2 button 1 (left button down)
;    Button 2 -- report as OS/2 button 1+2 (all buttons down)
;    Button 3 -- report as OS/2 button 2 (right button down)
;    Button 4 -- report as OS/2 button 1+2 (all buttons down)
;    Button 5 -- report as OS/2 button 1+2 (all buttons down)
;    Button 6 -- report as OS/2 button 1+2 (all buttons down)
;    Button 7 -- report as OS/2 button 1+2 (all buttons down)
;
; Notice that for two button mice no more than 3 buttons can be profitably
; used for OS/2.  Also note that this table must be exactly 128 bytes in
; length.
;

btnMap2  db       00000000b            ; 00000000
         db       00000010b            ; 00000001
         db       00001010b            ; 00000010
         db       00001010b            ; 00000011
         db       00001000b            ; 00000100
         db       00001010b            ; 00000101
         db       00001010b            ; 00000110
         db       00001010b            ; 00000111

         db       00001010b            ; 00001000
         db       00001010b            ; 00001001
         db       00001010b            ; 00001010
         db       00001010b            ; 00001011
         db       00001010b            ; 00001100
         db       00001010b            ; 00001101
         db       00001010b            ; 00001110
         db       00001010b            ; 00001111

         db       00001010b            ; 00010000
         db       00001010b            ; 00010001
         db       00001010b            ; 00010010
         db       00001010b            ; 00010011
         db       00001010b            ; 00010100
         db       00001010b            ; 00010101
         db       00001010b            ; 00010110
         db       00001010b            ; 00010111

         db       00001010b            ; 00011000
         db       00001010b            ; 00011001
         db       00001010b            ; 00011010
         db       00001010b            ; 00011011
         db       00001010b            ; 00011100
         db       00001010b            ; 00011101
         db       00001010b            ; 00011110
         db       00001010b            ; 00011111

         db       00001010b            ; 00100000
         db       00001010b            ; 00100001
         db       00001010b            ; 00100010
         db       00001010b            ; 00100011
         db       00001010b            ; 00100100
         db       00001010b            ; 00100101
         db       00001010b            ; 00100110
         db       00001010b            ; 00100111

         db       00001010b            ; 00101000
         db       00001010b            ; 00101001
         db       00001010b            ; 00101010
         db       00001010b            ; 00101011
         db       00001010b            ; 00101100
         db       00001010b            ; 00101101
         db       00001010b            ; 00101110
         db       00001010b            ; 00101111

         db       00001010b            ; 00110000
         db       00001010b            ; 00110001
         db       00001010b            ; 00110010
         db       00001010b            ; 00110011
         db       00001010b            ; 00110100
         db       00001010b            ; 00110101
         db       00001010b            ; 00110110
         db       00001010b            ; 00110111

         db       00001010b            ; 00111000
         db       00001010b            ; 00111001
         db       00001010b            ; 00111010
         db       00001010b            ; 00111011
         db       00001010b            ; 00111100
         db       00001010b            ; 00111101
         db       00001010b            ; 00111110
         db       00001010b            ; 00111111

         db       00001010b            ; 01000000
         db       00001010b            ; 01000001
         db       00001010b            ; 01000010
         db       00001010b            ; 01000011
         db       00001010b            ; 01000100
         db       00001010b            ; 01000101
         db       00001010b            ; 01000110
         db       00001010b            ; 01000111

         db       00001010b            ; 01001000
         db       00001010b            ; 01001001
         db       00001010b            ; 01001010
         db       00001010b            ; 01001011
         db       00001010b            ; 01001100
         db       00001010b            ; 01001101
         db       00001010b            ; 01001110
         db       00001010b            ; 01001111

         db       00001010b            ; 01010000
         db       00001010b            ; 01010001
         db       00001010b            ; 01010010
         db       00001010b            ; 01010011
         db       00001010b            ; 01010100
         db       00001010b            ; 01010101
         db       00001010b            ; 01010110
         db       00001010b            ; 01010111

         db       00001010b            ; 01011000
         db       00001010b            ; 01011001
         db       00001010b            ; 01011010
         db       00001010b            ; 01011011
         db       00001010b            ; 01011100
         db       00001010b            ; 01011101
         db       00001010b            ; 01011110
         db       00001010b            ; 01011111

         db       00001010b            ; 01100000
         db       00001010b            ; 01100001
         db       00001010b            ; 01100010
         db       00001010b            ; 01100011
         db       00001010b            ; 01100100
         db       00001010b            ; 01100101
         db       00001010b            ; 01100110
         db       00001010b            ; 01100111

         db       00001010b            ; 01101000
         db       00001010b            ; 01101001
         db       00001010b            ; 01101010
         db       00001010b            ; 01101011
         db       00001010b            ; 01101100
         db       00001010b            ; 01101101
         db       00001010b            ; 01101110
         db       00001010b            ; 01101111

         db       00001010b            ; 01110000
         db       00001010b            ; 01110001
         db       00001010b            ; 01110010
         db       00001010b            ; 01110011
         db       00001010b            ; 01110100
         db       00001010b            ; 01110101
         db       00001010b            ; 01110110
         db       00001010b            ; 01110111

         db       00001010b            ; 01111000
         db       00001010b            ; 01111001
         db       00001010b            ; 01111010
         db       00001010b            ; 01111011
         db       00001010b            ; 01111100
         db       00001010b            ; 01111101
         db       00001010b            ; 01111110
         db       00001010b            ; 01111111

;
;------------------------ Button Ordering Definitions ------------------------
;
;    This table contains the physical button reordering map.  The default
; mapping is no button reordering.  If the ORDER option is present, then it
; will dynamically construct the necessary mapping.  Also note that this
; table must be exactly 128 bytes in length.
;

btnOrder db       00000000b
         db       00000001b
         db       00000010b
         db       00000011b
         db       00000100b
         db       00000101b
         db       00000110b
         db       00000111b

         db       00001000b
         db       00001001b
         db       00001010b
         db       00001011b
         db       00001100b
         db       00001101b
         db       00001110b
         db       00001111b

         db       00010000b
         db       00010001b
         db       00010010b
         db       00010011b
         db       00010100b
         db       00010101b
         db       00010110b
         db       00010111b

         db       00011000b
         db       00011001b
         db       00011010b
         db       00011011b
         db       00011100b
         db       00011101b
         db       00011110b
         db       00011111b

         db       00100000b
         db       00100001b
         db       00100010b
         db       00100011b
         db       00100100b
         db       00100101b
         db       00100110b
         db       00100111b

         db       00101000b
         db       00101001b
         db       00101010b
         db       00101011b
         db       00101100b
         db       00101101b
         db       00101110b
         db       00101111b

         db       00110000b
         db       00110001b
         db       00110010b
         db       00110011b
         db       00110100b
         db       00110101b
         db       00110110b
         db       00110111b

         db       00111000b
         db       00111001b
         db       00111010b
         db       00111011b
         db       00111100b
         db       00111101b
         db       00111110b
         db       00111111b

         db       01000000b
         db       01000001b
         db       01000010b
         db       01000011b
         db       01000100b
         db       01000101b
         db       01000110b
         db       01000111b

         db       01001000b
         db       01001001b
         db       01001010b
         db       01001011b
         db       01001100b
         db       01001101b
         db       01001110b
         db       01001111b

         db       01010000b
         db       01010001b
         db       01010010b
         db       01010011b
         db       01010100b
         db       01010101b
         db       01010110b
         db       01010111b

         db       01011000b
         db       01011001b
         db       01011010b
         db       01011011b
         db       01011100b
         db       01011101b
         db       01011110b
         db       01011111b

         db       01100000b
         db       01100001b
         db       01100010b
         db       01100011b
         db       01100100b
         db       01100101b
         db       01100110b
         db       01100111b

         db       01101000b
         db       01101001b
         db       01101010b
         db       01101011b
         db       01101100b
         db       01101101b
         db       01101110b
         db       01101111b

         db       01110000b
         db       01110001b
         db       01110010b
         db       01110011b
         db       01110100b
         db       01110101b
         db       01110110b
         db       01110111b

         db       01111000b
         db       01111001b
         db       01111010b
         db       01111011b
         db       01111100b
         db       01111101b
         db       01111110b
         db       01111111b

;
;--------------------------- Three Button Mappings ---------------------------
;
;   This table contains the button mappings for three button mice.  The
; general button assignment strategy for three button mice is...
;
;    Button 1 -- report as OS/2 button 1 (left button down)
;    Button 2 -- report as OS/2 button 3 (middle button down)
;    Button 3 -- report as OS/2 button 2 (right button down)
;    Button 4 -- report as OS/2 button 1+2 chord (left + right buttons down)
;    Button 5 -- report as OS/2 button 1+3 chord (left + middle buttons down)
;    Button 6 -- report as OS/2 button 2+3 chord (middle + right buttons down)
;    Button 7 -- report as OS/2 button 1+2+3 chord (all buttons down)
;
;    Notice that for three button mice no more than 7 buttons can be
; profitably used for OS/2.  Also note that this table must be exactly 128
; bytes in length and further it must follow the start of the btnMap2 table
; by exactly 256 bytes.
;

btnMap3  db       00000000b            ; 00000000
         db       00000010b            ; 00000001
         db       00100000b            ; 00000010
         db       00100010b            ; 00000011
         db       00001000b            ; 00000100
         db       00001010b            ; 00000101
         db       00101000b            ; 00000110
         db       00101010b            ; 00000111

         db       00001010b            ; 00001000
         db       00001010b            ; 00001001
         db       00101010b            ; 00001010
         db       00101010b            ; 00001011
         db       00001010b            ; 00001100
         db       00001010b            ; 00001101
         db       00101010b            ; 00001110
         db       00101010b            ; 00001111

         db       00100010b            ; 00010000
         db       00100010b            ; 00010001
         db       00100010b            ; 00010010
         db       00100010b            ; 00010011
         db       00101010b            ; 00010100
         db       00101010b            ; 00010101
         db       00101010b            ; 00010110
         db       00101010b            ; 00010111

         db       00101010b            ; 00011000
         db       00101010b            ; 00011001
         db       00101010b            ; 00011010
         db       00101010b            ; 00011011
         db       00101010b            ; 00011100
         db       00101010b            ; 00011101
         db       00101010b            ; 00011110
         db       00101010b            ; 00011111

         db       00101000b            ; 00100000
         db       00101010b            ; 00100001
         db       00101000b            ; 00100010
         db       00101010b            ; 00100011
         db       00101000b            ; 00100100
         db       00101010b            ; 00100101
         db       00101000b            ; 00100110
         db       00101010b            ; 00100111

         db       00101010b            ; 00101000
         db       00101010b            ; 00101001
         db       00101010b            ; 00101010
         db       00101010b            ; 00101011
         db       00101010b            ; 00101100
         db       00101010b            ; 00101101
         db       00101010b            ; 00101110
         db       00101010b            ; 00101111

         db       00101010b            ; 00110000
         db       00101010b            ; 00110001
         db       00101010b            ; 00110010
         db       00101010b            ; 00110011
         db       00101010b            ; 00110100
         db       00101010b            ; 00110101
         db       00101010b            ; 00110110
         db       00101010b            ; 00110111

         db       00101010b            ; 00111000
         db       00101010b            ; 00111001
         db       00101010b            ; 00111010
         db       00101010b            ; 00111011
         db       00101010b            ; 00111100
         db       00101010b            ; 00111101
         db       00101010b            ; 00111110
         db       00101010b            ; 00111111

         db       00101010b            ; 01000000
         db       00101010b            ; 01000001
         db       00101010b            ; 01000010
         db       00101010b            ; 01000011
         db       00101010b            ; 01000100
         db       00101010b            ; 01000101
         db       00101010b            ; 01000110
         db       00101010b            ; 01000111

         db       00101010b            ; 01001000
         db       00101010b            ; 01001001
         db       00101010b            ; 01001010
         db       00101010b            ; 01001011
         db       00101010b            ; 01001100
         db       00101010b            ; 01001101
         db       00101010b            ; 01001110
         db       00101010b            ; 01001111

         db       00101010b            ; 01010000
         db       00101010b            ; 01010001
         db       00101010b            ; 01010010
         db       00101010b            ; 01010011
         db       00101010b            ; 01010100
         db       00101010b            ; 01010101
         db       00101010b            ; 01010110
         db       00101010b            ; 01010111

         db       00101010b            ; 01011000
         db       00101010b            ; 01011001
         db       00101010b            ; 01011010
         db       00101010b            ; 01011011
         db       00101010b            ; 01011100
         db       00101010b            ; 01011101
         db       00101010b            ; 01011110
         db       00101010b            ; 01011111

         db       00101010b            ; 01100000
         db       00101010b            ; 01100001
         db       00101010b            ; 01100010
         db       00101010b            ; 01100011
         db       00101010b            ; 01100100
         db       00101010b            ; 01100101
         db       00101010b            ; 01100110
         db       00101010b            ; 01100111

         db       00101010b            ; 01101000
         db       00101010b            ; 01101001
         db       00101010b            ; 01101010
         db       00101010b            ; 01101011
         db       00101010b            ; 01101100
         db       00101010b            ; 01101101
         db       00101010b            ; 01101110
         db       00101010b            ; 01101111

         db       00101010b            ; 01110000
         db       00101010b            ; 01110001
         db       00101010b            ; 01110010
         db       00101010b            ; 01110011
         db       00101010b            ; 01110100
         db       00101010b            ; 01110101
         db       00101010b            ; 01110110
         db       00101010b            ; 01110111

         db       00101010b            ; 01111000
         db       00101010b            ; 01111001
         db       00101010b            ; 01111010
         db       00101010b            ; 01111011
         db       00101010b            ; 01111100
         db       00101010b            ; 01111101
         db       00101010b            ; 01111110
         db       00101010b            ; 01111111

dseg     ends



;----------------------------------------------------------------------------#
;                                                                            #
;    This section contains all macros.                                       #
;                                                                            #
;----------------------------------------------------------------------------#

;
; Interface:
;
;    MyIoDelay()
;
; Description:
;
;    Performs machine-specific i/o delay.
;
; Notes:
;
;    This macro contains no mouse-specific code.
;

MyIoDelay macro

         push     ax                   ; Perform machine-specific
         DevIODelay <ax>               ;  i/o delay of at least
         pop      ax                   ;  0.5us

         endm



;----------------------------------------------------------------------------#
;                                                                            #
;    This section contains the inter-device communication (IDC) interface.   #
;                                                                            #
;----------------------------------------------------------------------------#

;
; Interface:
;
;    IdcEntry(FunctionCode AX, OurDs DS, CallersDs ES)
;
; Description:
;
;    Handle all requests from the attached device.  The commands supported
; are QueryConfig, EnableRead, DisableRead, EnableDevice and DisableDevice.
;
; Normal exit:
;
;    Carry clear.  Other registers set according to requested function.
;
; Error exit:
;
;    Carry set and ax non-zero.
;
; Side effects:
;
;    Stack is clean, registers ax, cx, dx, si and di are altered.  This
; routine contains no mouse-specific code.
;

dseg     segment

;
;    This table is used as a vector jump table to perform the functions
; requested of the IDC entry point.  The table is one-based with the table
; name biased to allow a zero-base index.
;

         align    2
         org      $-2
IdcFunc  label    word
         org      $+2
         dw       QueryConfig
         dw       EnableRead
         dw       DisableRead
         dw       EnableDevice
         dw       DisableDevice

dseg     ends

cseg     segment

         align    4

IdcEntry proc far

         cmp      ax,1                    ; Requested function must be
         jb       idcerr                  ;  in range of 1..5 and device
         cmp      ax,5                    ;  driver must be installed,
         ja       idcerr                  ;  otherwise we have an error
         test     devStatus,deinstalled   ;
         jnz      idcerr                  ;
         push     bx                      ;
         mov      bx,ax                   ; Call function using vector
         add      bx,bx                   ;  table -- return status set
         call     IdcFunc[bx]             ;  by function
         pop      bx                      ;
         ret                              ;
idcerr:  stc                              ; Report error (make sure ax is
         mov      ax,1                    ;  non-zero)
         ret                              ;

IdcEntry endp

cseg     ends



;
; Interface:
;
;    QueryConfig(DataBuffer *ES:[DI])
;
; Description:
;
;    Write the mouse hardware device configuration to the data buffer.
;
; Exit:
;
;    Carry clear.
;
; Side effects:
;
;    Stack is clean, registers cx, si and di are altered.  This
; routine contains no mouse-specific code.
;

cseg     segment

         align    4

QueryConfig proc near

         mov      cx,es:[di].cfgDataLen   ; Get the amount of requested
         cmp      cx,cfgDatLen            ;  data -- limited to cfgDatLen
         jbe      qcfg1                   ;  bytes.
         mov      cx,cfgDatLen            ;
qcfg1:   mov      si,offset deviceData    ;
         cld                              ; Copy requested data to the
     rep movsb                            ;  buffer.
         clc                              ;
         ret                              ;

QueryConfig endp

cseg     ends



;
; Interface:
;
;    EnableRead(InterruptPacketData *ES:[DI])
;
; Description:
;
;    Enables the interrupt data packets to be reported to MOUSE$.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set and ax non-zero.
;
; Side effects:
;
;    Stack is clean, registers ax, bx, cx, dx and si are altered.  This
; routine contains no mouse-specific code.
;

cseg     segment

         align    4

EnableRead proc near

         cli                              ;
         test     devStatus,attached      ; If already attached to MOUSE$
         jnz      enrd1                   ;  then go enable
         mov      intPacketOff,di         ;
         mov      bx,offset mseName       ; Attach MOUSE$
         mov      di,offset mseDd         ;
         mov      dl,DevHlp_AttachDd      ;
         call     deviceHelp              ;
         mov      di,intPacketOff         ;
         jc       enrd2                   ;
         or       devStatus,attached      ;
enrd1:   sti                              ;
         or       devStatus,readEnable    ; Enable interrupt reporting
         ret                              ;
enrd2:   sti                              ; Return error condition
         mov      ax,1                    ;
         ret                              ;

EnableRead endp

cseg     ends



;
; Interface:
;
;    DisableRead()
;
; Description:
;
;    Disables the reporting of interrupt to MOUSE$.
;
; Exit:
;
;    Carry clear.
;
; Side effects:
;
;    Stack is clean, no registers are altered.  This routine contains no
; mouse-specific code.
;

cseg     segment

         align    4

DisableRead proc near

         or       devStatus,not readEnable   ; Disable interrupt reporting
         clc                                 ;
         ret                                 ;

DisableRead endp

cseg     ends



;
; Interface:
;
;    EnableDevice()
;
; Description:
;
;    Enables device interrupts from 8259.
;
; Exit:
;
;    Carry clear.
;
; Side effects:
;
;    Stack is clean, register ax is altered.  This routine contains no
; mouse-specific code.
;

cseg     segment

         align    4

EnableDevice proc near

         pushf                            ;
         push     dx                      ;
         cmp      protocol,protoNM        ;
         jz       enDevDon                ;
         cli                              ; Disable cpu interrupts
         mov      dx,mskPrt8259           ;
         in       al,dx                   ;
         and      al,enable8259           ; Enable mouse interrupts at 8259
         out      dx,al                   ;
         MyIoDelay                        ;
enDevDon:or       devStatus,deviceEnabled ; Flag enabled state
         pop      dx                      ;
         popf                             ; Restore previous cpu interrupt
         xor      ax,ax                   ;  state
         ret                              ;

EnableDevice endp

cseg     ends



;
; Interface:
;
;    DisableDevice()
;
; Description:
;
;    Disables device interrupts from 8259.
;
; Exit:
;
;    Carry clear.
;
; Side effects:
;
;    Stack is clean, register ax is altered.  This routine contains no
; mouse-specific code.
;

cseg     segment

         align    4

DisableDevice proc near

         pushf                                  ;
         push     dx                            ;
         cmp      protocol,protoNM              ;
         jnz      dsDevDon                      ;
         cli                                    ; Disable cpu interrupts
         mov      dx,mskPrt8259                 ;
         in       al,dx                         ;
         or       al,disable8259                ; Disable interrupts at 8259
         out      dx,al                         ;
         MyIoDelay                              ;
dsDevDon:and      devStatus,not deviceEnabled   ; Flag disabled state
         pop      dx                            ;
         popf                                   ; Restore previous cpu
         xor      ax,ax                         ;  interrupt state
         ret                                    ;

DisableDevice endp

cseg     ends



;----------------------------------------------------------------------------#
;                                                                            #
;    This section contains the general strategy interface.                   #
;                                                                            #
;----------------------------------------------------------------------------#

;
; Interface:
;
;    CmdFunc(Packet *es:[bx])
;
; Description:
;
;    Handle all strategy requests to the device.
;
; Normal exit:
;
;    The request packet status field is set to indicate function complete
; with no error.
;
; Error exit:
;
;    The error code is set in the request packet.
;
; Side effects:
;
;    Registers are altered.
;

dseg     segment

;
;    This table is used as a vector jump table to perform the functions
; requested of the strategy entry point.  The table is zero-based.
;

         align    2

CmdFunc  label    word

         dw       DoCmdInit            ;  0 - CmdInit
         dw       BadCommand           ;  1 - CmdMedChk
         dw       BadCommand           ;  2 - CmdBldBPB
         dw       BadCommand           ;  3 - CmdIOCTLR
         dw       BadCommand           ;  4 - CmdINPUT
         dw       BadCommand           ;  5 - CmdNDR
         dw       BadCommand           ;  6 - CmdInputS
         dw       BadCommand           ;  7 - CmdInputF
         dw       BadCommand           ;  8 - CmdOUTPUT
         dw       BadCommand           ;  9 - CmdOUTPUTV

         dw       BadCommand           ; 10 - CmdOutputS
         dw       BadCommand           ; 11 - CmdOutputF
         dw       BadCommand           ; 12 - CmdIOCTLW
         dw       DoCmdOpen            ; 13 - CmdOpen
         dw       DoCmdClose           ; 14 - CmdClose
         dw       BadCommand           ; 15 - CmdRemMed
         dw       BadCommand           ; 16 - CmdGenIOCTL
         dw       BadCommand           ; 17 - CmdResMed
         dw       BadCommand           ; 18 - CmdGetLogMap
         dw       BadCommand           ; 19 - CmdSetLogMap

         dw       DoCmdDeInstall       ; 20 - CmdDeInstall
         dw       BadCommand           ; 21 -
         dw       BadCommand           ; 22 - CmdPartfixeddisks
         dw       BadCommand           ; 23 - CmdGetfd_logunitsmap
         dw       BadCommand           ; 24 - CmdInputBypass
         dw       BadCommand           ; 25 - CmdOutputBypass
         dw       BadCommand           ; 26 - CmdOutputBypassV
         dw       BadCommand           ; 27 - CmdInitBase
         dw       BadCommand           ; 28 - CmdShutdown
         dw       BadCommand           ; 29 - CmdGetDevSupport

         dw       BadCommand           ; 30 -
         dw       DoCmdInitComplete    ; 31 - CmdInitComplete
         dw       BadCommand           ; 32 -
         dw       BadCommand           ; 33 -
         dw       BadCommand           ; 34 -
         dw       BadCommand           ; 35 -
         dw       BadCommand           ; 36 -
         dw       BadCommand           ; 37 -
         dw       BadCommand           ; 38 -
         dw       BadCommand           ; 39 -

         dw       BadCommand           ; 40 -
         dw       BadCommand           ; 41 -
         dw       BadCommand           ; 42 -
         dw       BadCommand           ; 43 -
         dw       BadCommand           ; 44 -
         dw       BadCommand           ; 45 -
         dw       BadCommand           ; 46 -
         dw       BadCommand           ; 47 -
         dw       BadCommand           ; 48 -
         dw       BadCommand           ; 49 -

         dw       BadCommand           ; 50 -
         dw       BadCommand           ; 51 -
         dw       BadCommand           ; 52 -
         dw       BadCommand           ; 53 -
         dw       BadCommand           ; 54 -
         dw       BadCommand           ; 55 -
         dw       BadCommand           ; 56 -
         dw       BadCommand           ; 57 -
         dw       BadCommand           ; 58 -
         dw       BadCommand           ; 59 -

         dw       BadCommand           ; 60 -
         dw       BadCommand           ; 61 -
         dw       BadCommand           ; 62 -
         dw       BadCommand           ; 63 -
         dw       BadCommand           ; 64 -
         dw       BadCommand           ; 65 -
         dw       BadCommand           ; 66 -
         dw       BadCommand           ; 67 -
         dw       BadCommand           ; 68 -
         dw       BadCommand           ; 69 -

         dw       BadCommand           ; 70 -
         dw       BadCommand           ; 71 -
         dw       BadCommand           ; 72 -
         dw       BadCommand           ; 73 -
         dw       BadCommand           ; 74 -
         dw       BadCommand           ; 75 -
         dw       BadCommand           ; 76 -
         dw       BadCommand           ; 77 -
         dw       BadCommand           ; 78 -
         dw       BadCommand           ; 79 -

         dw       BadCommand           ; 80 -
         dw       BadCommand           ; 81 -
         dw       BadCommand           ; 82 -
         dw       BadCommand           ; 83 -
         dw       BadCommand           ; 84 -
         dw       BadCommand           ; 85 -
         dw       BadCommand           ; 86 -
         dw       BadCommand           ; 87 -
         dw       BadCommand           ; 88 -
         dw       BadCommand           ; 89 -

         dw       BadCommand           ; 90 -
         dw       BadCommand           ; 91 -
         dw       BadCommand           ; 92 -
         dw       BadCommand           ; 93 -
         dw       BadCommand           ; 94 -
         dw       BadCommand           ; 95 -
         dw       BadCommand           ; 96 -
         dw       BadCommand           ; 97 - CmdAddOnPrep
         dw       BadCommand           ; 98 - CmdStar
         dw       BadCommand           ; 99 - CmdStop

dseg     ends

cseg     segment

         align    4

CmdEntry proc far

         movzx    si,es:[bx].PktCmd             ;
         cmp      si,99                         ; Requested function must be
         ja       cmdErr                        ;  in range of 0..99
         add      si,si                         ;
         call     CmdFunc[si]                   ;
         or       es:[bx].PktStatus,stdOn       ; Handle requested function
         ret                                    ;
cmdErr:  mov      es:[bx].PktStatus,unknownCmd  ;
         or       es:[bx].PktStatus,stdOn       ; Unsupported function
         ret                                    ;

CmdEntry endp

cseg     ends



;
; Interface:
;
;    DoCmdInit()
;
; Description:
;
;    Initializes mouse.  Status returned in ax.
;
; Normal exit:
;
;    Device is initialized, ax zero.
;
; Error exit:
;
;    Device disabled, ax non-zero.
;
; Side effects:
;
;    Registers are altered.
;

dseg     segment

saveEs   dw       0
saveBx   dw       0

dseg     ends

cseg     segment

         align    4

DoCmdInit proc near

         cmp      word ptr deviceHelp+2,0          ; Initialize mouse if not
         jnz      initDone                         ;  already initialized

         mov      saveBx,bx                        ;
         mov      saveEs,es                        ;

         mov      es:[bx].PktStatus,00h            ; Clear req block status

         mov      ax,word ptr es:[bx].initDevHlp   ;
         mov      word ptr deviceHelp,ax           ; Save pointer to
         mov      ax,word ptr es:[bx].initDevHlp+2 ;  DeviceHelp entry point
         mov      word ptr deviceHelp+2,ax         ;

         mov      al,globalInfoSeg                 ; Get segment address of
         mov      dl,DevHlp_GetDOSVar              ;  Global info table
         call     deviceHelp                       ;
         jc       initErr                          ;
         mov      es,ax                            ;
         mov      ax,word ptr es:[bx]              ; Save GDT information
         mov      word ptr globalInfo+0,0          ;  address
         mov      word ptr globalInfo+2,ax         ;

         mov      al,localInfoAddr                 ; Get address of local
         mov      dl,DevHlp_GetDOSVar              ;  info table, this is
         call     deviceHelp                       ;  different for each
         jc       initErr                          ;  task, so only the
         mov      word ptr localInfoPtr+0,bx       ;  address of the pointer
         mov      word ptr localInfoPtr+2,ax       ;  is saved

         mov      al,0bh                           ;
         mov      bl,1                             ; Find out if we are
         xor      dh,dh                            ;  running on an ABIOS
         mov      dl,DevHlp_GetLIDEntry            ;  machine by requesting
         call     DeviceHelp                       ;  a LID.  If we get one
         jc       init1                            ;  give it back because
         mov      dl,DevHlp_FreeLIDEntry           ;  we don't really want
         call     DeviceHelp                       ;  one.  If we get any
         jmp      short init2                      ;  other error than ABIOS
init1:   cmp      ax,Error_ABIOS_Not_Present       ;  not present assume that
         jz       init3                            ;  ABIOS is present.
init2:   mov      isAbios,1                        ;
init3:
         mov      es,saveEs                        ;
         mov      bx,saveBx                        ; Restore packet pointer

         call     CheckOptions                     ;
         jc       initErr                          ; Check options, get the
         call     FindMouse                        ;  mouse port address and
         jc       initErr                          ;  interrupt number
         call     GetIrq                           ;
         call     GetIrqOwnership                  ; Get interrupt ownership
         jc       initErr                          ;  exit if error
         call     DisableDevice                    ; Disable device
         call     GetPortOwnership                 ; Get BIOS/ABIOS ownership
         call     SetupDevice                      ;
         call     EnableDevice                     ; Return success
         call     Report                           ;
         mov      es,saveEs                        ;
         mov      bx,saveBx                        ; Set length of driver to
         mov      byte ptr es:[bx].InitcUnit,0     ;  discard initialization
         mov      ax,offset endCode                ;  code and data
         mov      word ptr es:[bx].InitpEnd,ax     ;
         mov      ax,offset endData                ;
         mov      word ptr es:[bx].InitpEnd+2,ax   ;
         xor      ax,ax                            ;
         ret                                       ;
initDone:mov      es:[bx].PktStatus,unknownCmd     ; Mouse is already
         ret                                       ;  initialized
initErr: mov      es,saveEs                        ;
         mov      bx,saveBx                        ; Set error status in
         or       es:[bx].PktStatus,initBad        ;  packet and set length
         mov      byte ptr es:[bx].InitcUnit,0     ;  of driver to zero to
         mov      word ptr es:[bx].InitpEnd,0      ;  release all storage
         mov      word ptr es:[bx].InitpEnd+2,0    ;
         mov      ax,1                             ; Return error condition
         ret                                       ;

DoCmdInit endp

cseg     ends



;
; Interface:
;
;    DoCmdDeInstall()
;
; Description:
;
;    Deinstalls the attached mouse device.
;
; Side effects:
;
;    Stack is clean, registers ax, dx and di are altered.  This routine is
; mouse-specific.
;

cseg     segment

         align    4

DoCmdDeInstall proc near

         cmp      word ptr deviceHelp+2,0       ;
         jz       deinst7                       ; Deinstall device -- can be
         test     devStatus,deinstalled         ;  done only once and after
         jnz      deinst7                       ;  device driver is loaded

         push     bx                            ;
         push     es                            ;
         test     devStatus,attached            ; If attached to MOUSE$
         jz       deinst0                       ;  then go tell him to
         mov      ax,ds                         ;  disable reading
         mov      es,ax                         ;
         mov      ax,0002h                      ;
         push     ds                            ;
         mov      ds,mseDd.protDs               ;
         call     es:mseDd.protEntry            ;
         pop      ds                            ;

deinst0: cmp      protocol,protoBS              ;
         jz       deinst3                       ; Deinstallation is device
         cmp      protocol,protoIN              ;  dependent
         jz       deinst4                       ;
         cmp      protocol,protoPS              ;
         jz       deinst5                       ;
         cmp      protocol,protoNM              ;
         jnz      deinst1                       ;

         pop      es                            ; Flag uninstalled state,
         pop      bx                            ;  set Packet Complete Flag
         or       devStatus,deinstalled         ;  and return
         ret                                    ;

deinst1: cmp      deviceData.comPortNum,0       ; If not an BIOS/ABIOS com port
         jz       deinst6                       ;  then nothing to do
         cmp      gotLid,1                      ;
         jnz      deinst2                       ; If port allocated from ABIOS
         mov      ax,devLid                     ;  then release it
         mov      dl,DevHlp_FreeLIDEntry        ;
         call     deviceHelp                    ;
deinst2: push     es                            ;
         mov      ax,biosData                   ; Release com port for others
         mov      es,ax                         ;  by replacing first port address
         mov      di,pComAddr                   ;  in 0040:xxxx area
         mov      ax,deviceData.portAddr        ;
         mov      word ptr es:[di],ax           ;
         pop      es                            ;
         jmp      short deinst6                 ;

deinst3: mov      dx,deviceData.portAddr        ;
         add      dx,busCtrl                    ; Disable interrupts from
         mov      al,irqDsbl                    ;  buss mouse
         out      dx,al                         ;
         jmp      short deinst6                 ;

deinst4: mov      dx,deviceData.portAddr        ;
         mov      al,inpMode                    ; Disable interrupts from
         out      dx,al                         ;  Inport mouse
         MyIoDelay                              ;
         mov      al,inpHz0I1                   ;
         add      dx,inpData - inpAddr          ;
         out      dx,al                         ;
         jmp      short deinst6                 ;

deinst5: call     DisableKeyboardIrq            ;
         mov      al,ps2DsKey                   ; Disable interrupts from
         call     SendPs2Control                ;  PS/2 mouse
         mov      al,pixDisbl                   ;
         call     SendPs2Command                ;
         call     GetCommandByte                ;
         and      al,not (ps2AuxDs+ps2KeyDs+ps2AuxEn+ps2KeyEn)
         or       al,ps2AuxDs+ps2KeyEn          ;
         call     PutCommandByte                ;
         call     EnableKeyboardIrq             ;

         cmp      gotLid,1                      ;
         jnz      deinst6                       ; If device allocated from
         mov      ax,devLid                     ;  ABIOS then release it
         mov      dl,DevHlp_FreeLIDEntry        ;
         call     deviceHelp                    ;

deinst6: mov      bl,deviceData.Irq             ;
         xor      bh,bh                         ; Release Interrupt vector
         mov      dl,DevHlp_UnsetIrq            ;
         call     deviceHelp                    ;
         pop      es                            ;
         pop      bx                            ; Flag uninstalled state,
         or       devStatus,deinstalled         ;  set Packet Complete Flag
         ret                                    ;  and return

deinst7: mov      es:[bx].PktStatus,unknownCmd  ;
         ret                                    ;

DoCmdDeInstall endp

cseg     ends



;
; Interface:
;
;    DoCmdOpen()
;
; Description:
;
;    Called when DosOpen() attempts to open the device.  The device may
; not be shared, exactly one outstanding open is allowed.
;

cseg     segment

         align    4

DoCmdOpen proc near

         push     bx                            ;
         push     es                            ; Get PID of current task
         les      bx,localInfoPtr               ;
         les      bx,dword ptr es:[bx]          ;
         mov      ax,es:[bx].LIS_CurProcID      ;
         pop      es                            ;
         pop      bx                            ;

         mov      es:[bx].PktStatus,unknownCmd  ;
         ret                                    ;

DoCmdOpen endp

cseg     ends



;
; Interface:
;
;    DoCmdClose()
;
; Description:
;
;    Called when DosClose() attempts to close the device or when the device
; is open and the processing owning the open handle terminates for any
; reason.
;

cseg     segment

         align    4

DoCmdClose proc near

         mov      es:[bx].PktStatus,unknownCmd  ;
         ret                                    ;

DoCmdClose endp

cseg     ends



;
; Interface:
;
;    DoCmdInitComplete()
;
; Description:
;
;    Called once ALL physical devices are loaded and initialized.  Used to
; reset the buffering in case some other driver changed it during their
; setup -- this happens when checking a serial port because that is the
; only serial port parameter which cannot be accurately saved.
;
; Side effects:
;
;    Stack is clean, registers ax and dx are altered.  This routine is
; mouse-specific.
;

cseg     segment

         align    4

DoCmdInitComplete proc near


         cmp      word ptr deviceHelp+2,0       ;
         jz       initd2                        ; Finish initializating at
         test     devStatus,deinstalled         ;  ring 0 after all physical
         jnz      initd2                        ;  device drivers loaded
         cmp      buffered,0                    ;
         jz       initd1                        ;
         mov      al,fcrTgr01+fcrBfrd           ; If buffering is enabled then
         mov      dx,deviceData.portAddr        ;  set buffering mode again
         add      dx,fcr                        ;
         out      dx,al                         ;
initd1:  ret                                    ;
initd2:  mov      es:[bx].PktStatus,unknownCmd  ;
         ret                                    ;

DoCmdInitComplete endp

cseg     ends



;
; Interface:
;
;    BadCommand()
;
; Description:
;
;    Handler for illegal or unsupported commands.
;

cseg     segment

         align    4

BadCommand proc near

         mov      es:[bx].PktStatus,unknownCmd  ; Unsupported or illegal
         ret                                    ;  command

BadCommand endp

cseg     ends



;----------------------------------------------------------------------------#
;                                                                            #
;    This section contains all interrupt handlers.                           #
;                                                                            #
;----------------------------------------------------------------------------#

;
; Interface:
;
;    InterruptHandler()
;
; Description:
;
;    The mouse interrupt handler entry points follow.  The actual interrupt
; handler used depends on the communication protocol.  These handlers will
; eat mouse packets which don't change from the previous packet -- it is
; more efficient to ignore them here than at a higher level in the operating
; system.
;
; Exit:
;
;    Carry clear.
;
; Side effects:
;
;    Stack is clean, registers ax, bx, cx, dx, si, di and es are altered.
; These routines are communication protocol specific.
;
; Notes:
;
;    The device dependent packet is converted into either a relative device
; independent packet or an absolute device independent packet.  The relative
; packet has the format...
;
;    Word 0: Event status
;       0000 0000 0000 0000 - no button pressed and no motion
;       0000 0000 0000 0001 - motion only
;       0000 0000 0... .010 - button 1 pressed and mouse moved
;       0000 0000 0..0 1..0 - button 2 pressed and mouse moved
;       0000 0000 001. ...0 - button 3 pressed and mouse moved
;       0000 0000 0... .100 - button 1 pressed and mouse not moved
;       0000 0000 0..1 0..0 - button 2 pressed and mouse not moved
;       0000 0000 010. ...0 - button 3 pressed and mouse not moved
;
;    Word 1: Column movement (delta-X) in mickeys (signed 2's complement)
;
;    Word 2: Row movement (delta-Y) in mickeys (signed 2's complement)
;
; The absolute packet has the format...
;
;    Word 0: Event status
;       0000 0000 0000 0000 - no button pressed and no motion
;       0000 0000 0000 0001 - motion only
;       0000 0000 0... .010 - button 1 pressed and mouse moved
;       0000 0000 0..0 1..0 - button 2 pressed and mouse moved
;       0000 0000 001. ...0 - button 3 pressed and mouse moved
;       0000 0000 0... .100 - button 1 pressed and mouse not moved
;       0000 0000 0..1 0..0 - button 2 pressed and mouse not moved
;       0000 0000 010. ...0 - button 3 pressed and mouse not moved
;
;    Word 1: Row position, top = 0
;
;    Word 2: Column position, left = 0
;
;    Word 3: Number of row positions (maximum row position?)
;
;    Word 4: Number of column positions (maximum column position?)
;

dseg     segment

lastEvnt dw       00000h
lastRow  dw       0ffffh
lastCol  dw       0ffffh
accelrat dw       NoAccelerate
accelFix dw       0
accelTbl db       -7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7
outOfRng db       1
ps2Sync  db       0
ps2First db       1
ps2Mask  db       0
intrMap  db       32 dup(0)

dseg     ends

cseg     segment

         align    4

InterruptHandler  equ   $

cseg     ends



;
; Interface:
;
;    InterruptHandlerBS()
;
; Description:
;
;    Handles mouse interrupts for the buss mouse.  Packet bytes are
; translated to common relative event format and then sent to the device
; independent event handler.  Unlike the serial protocols, all information
; required is available when an interrupt is received -- therefore the
; packet is utilized immediately and is NOT accumulated.
;
; Notes:
;
;    The buss mouse returns a 4 byte packet, and has the format...
;
;    Byte 0: .... xxxx Low-order nibble of delta-X (column)
;
;    Byte 1: .... xxxx High-order nibble of delta-X (column)
;
;    Byte 2: .... xxxx Low-order nibble of delta-Y (row)
;
;    Byte 3: .... xxxx High-order nibble of delta-Y (row)
;            L... .... Button 1 not pressed
;            .M.. .... Button 3 not pressed
;            ..R. .... Button 2 not pressed
;

cseg     segment

         align    4

InterruptHandlerBS proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ; Call DeviceHelp to issue eoi
         mov      dl,DevHlp_Eoi           ;
         call     deviceHelp              ;
         mov      di,deviceData.portAddr  ;
         mov      dx,di                   ; Get di as mouse data port and
         add      dx,busCtrl              ;  dx as mouse request port
         mov      al,lowX                 ;
         out      dx,al                   ; Read low-order and high-order
         MyIODelay                        ;  nibbles of deltaX and combine
         xchg     dx,di                   ;  them in bh for column movement
         in       al,dx                   ;
         xchg     dx,di                   ;
         and      al,0fh                  ;
         mov      bh,al                   ;
         mov      al,hiX                  ;
         out      dx,al                   ;
         MyIODelay                        ;
         xchg     dx,di                   ;
         in       al,dx                   ;
         xchg     dx,di                   ;
         shl      al,4                    ;
         or       bh,al                   ;
         sar      bx,8                    ;
         mov      si,bx                   ; Put delta-X temporarily into si
         xor      si,colRvFlg             ;
         add      si,colRvVal             ;
         mov      al,lowY                 ;
         out      dx,al                   ;
         MyIODelay                        ; Read low-order and high-order
         xchg     dx,di                   ;  nibbles of delta-Y and combine
         in       al,dx                   ;  then in bl for row movement
         xchg     dx,di                   ;
         and      al,0fh                  ;
         mov      bh,al                   ;
         mov      al,hiY                  ;
         out      dx,al                   ; Place button status in bl for
         MyIODelay                        ;  later
         xchg     dx,di                   ;
         in       al,dx                   ; Reset mouse request port
         xchg     dx,di                   ;
         mov      bl,al                   ; Put delta-Y into bx, and delta-X
         shl      al,4                    ;  in dx with si duplicating bx
         or       bh,al                   ;
         xor      al,al                   ; Get button key press translation
         out      dx,al                   ;  Get event flags in cx
         mov      dx,si                   ;   if button pressed and moved
         movsx    si,bh                   ;     0000 0000 00M0 R0L0
         xor      si,rowRvFlg             ;
         add      si,rowRvVal             ;
         shr      bl,5                    ;   if button pressed only
         xor      bh,bh                   ;     0000 0000 0M0R 0L00
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,si,cl                ;
         shld     si,ax,cl                ;
         movzx    cx,intrMap[bx]          ;   if move only
         mov      bx,si                   ;     0000 0000 0000 0001
         or       si,dx                   ;   if no button pressed & not moved
         jnz      intrBS1                 ;     0000 0000 0000 0000
         add      cx,cx                   ;
         cmp      cx,lastEvnt             ;
         jnz      intrBS2                 ;
         jmp      short intrBS3           ;  Where L, R and M are 1 if the
intrBS1: cmp      cx,1                    ;   corresponding button was pressed
         adc      cx,0                    ;   and 0 otherwise
intrBS2: mov      lastEvnt,cx             ;
         test     devStatus,readEnable    ;
         jz       intrBS3                 ; If reporting disabled then exit
         mov      si,intPacketOff         ;
         push     ds                      ; Set up packet addressing
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
intrBS3: cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts and reenable
         in       al,dx                   ;  mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;

InterruptHandlerBS endp

cseg     ends




;
; Interface:
;
;    InterruptHandlerIN()
;
; Description:
;
;    Handles mouse interrupts for the Inport buss mouse.  Packet bytes are
; translated to common relative event format and then sent to the device
; independent event handler.  Unlike the serial protocols, all information
; required is available when an interrupt is received -- therefore the
; packet is utilized immediately and is NOT accumulated.
;
; Notes:
;
;    The Inport buss mouse returns either a 1 or 3 byte packet, and has the
; format...
;
;    Byte 0: .x.. .... Movement status (1 = movement, bytes 2 and 3 present)
;            .... .L.. Button 1 down
;            .... ..M. Button 2 down
;            .... ...R Button 3 down
;
;    Byte 1: xxxx xxxx Delta-X, only defined if movement flag set (column)
;
;    Byte 2: xxxx xxxx Delta-Y, only defined if movement flag set (row)
;

cseg     segment

         align    4

InterruptHandlerIN proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ; Call DeviceHelp to issue eoi
         mov      dl,DevHlp_Eoi           ;
         call     deviceHelp              ;
         mov      di,deviceData.portAddr  ; Get di as mouse base port and
         mov      dx,di                   ;  dx as register port
         mov      al,inpMode              ;
         out      dx,al                   ; Latch data by setting hold flag
         MyIODelay                        ;
         add      dx,inpData              ;
         in       al,dx                   ;
         MyIODelay                        ;
         or       al,inpHoldE             ;
         out      dx,al                   ;
         xchg     dx,di                   ;
         mov      al,inpStat              ;
         out      dx,al                   ; Get first byte of packet (status)
         MyIODelay                        ;
         xchg     dx,di                   ;
         in       al,dx                   ;
         xchg     dx,di                   ;
         mov      bl,al                   ;
         and      bx,0007h                ; Get button key press translation
         movzx    cx,intrMap[bx]          ;
         xor      bx,bx                   ;
         xor      si,si                   ; If movement flag not set then
         test     al,inpMovmt             ;  delta-X and delta-Y are zero
         jz       intrIN1                 ;
         mov      al,inpData1             ;
         out      dx,al                   ; Get delta-X in dx
         MyIODelay                        ;
         xchg     dx,di                   ;
         in       al,dx                   ;
         xchg     dx,di                   ;
         movsx    si,al                   ;
         xor      si,colRvFlg             ;
         add      si,colRvVal             ;
         mov      al,inpData2             ;
         out      dx,al                   ; Get delta-Y in bx
         MyIODelay                        ;
         xchg     dx,di                   ;
         in       al,dx                   ;
         xchg     dx,di                   ;
         movsx    bx,al                   ;
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ;
intrIN1: mov      al,inpMode              ;
         out      dx,al                   ; Turn hold flag off
         MyIODelay                        ;
         xchg     dx,di                   ;
         in       al,dx                   ;
         MyIODelay                        ;
         and      al,inpHoldD             ;
         out      dx,al                   ;
         mov      dx,si                   ;
         mov      si,cx                   ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;
         or       ax,dx                   ;
         jnz      intrIN2                 ;
         add      cx,cx                   ;
         cmp      cx,lastEvnt             ;
         jnz      intrIN3                 ;
         jmp      short intrIN4           ;
intrIN2: cmp      cx,1                    ;
         adc      cx,0                    ;
intrIN3: mov      lastEvnt,cx             ;
         test     devStatus,readEnable    ;
         jz       intrIN4                 ; If reporting disabled then exit
         mov      si,intPacketOff         ;
         push     ds                      ; Set up packet addressing
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
intrIN4: cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts and reenable
         in       al,dx                   ;  mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;

InterruptHandlerIN endp

cseg     ends




;
; Interface:
;
;    InterruptHandlerPS()
;
; Description:
;
;    Handles interrupts for IBM and Logitech mice using the PS/2
; communication protocol.  Packet bytes are accumulated until the packet is
; complete.  The packet is translated to common relative event format and
; then sent to the device independent event handler.
;
; Notes:
;
;    Unlike other mice, PS/2 mice are attached to the keyboard controller and
; therefore use a fixed port address and interrupt address.  This constraint
; is reflected in this interrupt handler.  Any values explicitly provided by
; the user are ignored.  The PS/2 communication protocol returns a 3 byte
; packet, and has the format...
;
;    Byte 0: Y... .... Y data overflow bit, 1 = overflow
;            .X.. .... X data overflow bit, 1 = overflow
;            ..Y. .... High order bit of Delta-Y (signed 2's complement)
;            ...X .... High order bit of Delta-X (signed 2's complement)
;            .... 1... Sync byte, marks first byte of packet
;            .... .M.. Middle button (button 3) status, 1 = depressed
;            .... ..R. Right button  (button 2) status, 1 = depressed
;            .... ...L Left button   (button 1) status, 1 = depressed
;
;    Byte 1: XXXX XXXX Delta-X (column)
;
;    Byte 2: YYYY YYYY Delta-Y (row)
;

cseg     segment

         align    4

InterruptHandlerPS proc far

         sti                              ; Enable system interrupts
         in       al,ps2Stat              ;
         mov      ah,al                   ; Read the status register, if the
         and      al,ps2AxFl+ps2Otpt      ;  interrupt is not for the
         cmp      al,ps2AxFl+ps2Otpt      ;  auxilary device or data is not
         jz       intrPS0                 ;  available then this is not our
         stc                              ;  interrupt so pass it on
         ret                              ;
intrPS0: cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         in       al,ps2Data              ; Read the data value, if a timeout
         test     ah,ps2Time              ;  has occurred then ignore the
         jnz      intrPS5                 ;  data
         mov      di,byteCnt              ;
         or       di,di                   ;
         jnz      intrPS2                 ; First packet must be used to
         cmp      ps2First,1              ;  determine the sync flags used
         jnz      intrPS1                 ;
         mov      ps2First,0              ;
         mov      ah,ps2Mask              ; If not accumulating packet and
         and      ah,al                   ;  not first byte of packet then
         mov      ps2Sync,ah              ;  discard byte
intrPS1: mov      ah,ps2Mask              ;
         and      ah,al                   ;
         cmp      ah,ps2Sync              ;
         jnz      intrPS6                 ;
intrPS2: mov      mEvent[di],al           ;
         inc      di                      ; Accumulate packet bytes in
         mov      byteCnt,di              ;  mEvent.  Incomplete packets and
         cmp      di,sizPktPS             ;  spurious data will be discarded.
         jc       intrPS6                 ;
         test     devStatus,readEnable    ; If reporting disabled then do
         jz       intrPS5                 ;  nothing
         mov      bl,mEvent[0]            ; Get button key press translation
         and      bx,0007h                ; Put delta-X into dx and delta-Y
         movzx    si,intrMap[bx]          ;  into bx
         bt       word ptr mEvent[0],4    ; Get event flags in cx
         sbb      dx,dx                   ;  if button pressed and moved
         mov      dl,mEvent[1]            ;    0000 0000 00M0 R0L0
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         bt       word ptr mEvent[0],5    ;  if button pressed only
         sbb      bx,bx                   ;    0000 0000 0M0R 0L00
         mov      bl,mEvent[2]            ;  if move only
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;    0000 0000 0000 0001
         or       ax,dx                   ;  if no button pressed & not moved
         jnz      intrPS3                 ;    0000 0000 0000 0000
         add      cx,cx                   ;
         cmp      cx,lastEvnt             ;
         jnz      intrPS4                 ;
         jmp      short intrPS5           ; Where L, R and M are 1 if the
intrPS3: cmp      cx,1                    ;  corresponding button was pressed
         adc      cx,0                    ;  and 0 otherwise
intrPS4: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ;
         push     ds                      ; Set up packet addressing
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
intrPS5: mov      byteCnt,0               ;
intrPS6: mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
         cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ; Enable system interrupts
         clc                              ; Indicate our interrupt
         ret                              ;

InterruptHandlerPS endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerMP()
;
; Description:
;
;    Handles interrupts for the MP communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common relative event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    The MP communication protocol returns either a 3 or 4 byte packet, and
; has the format...
;
;    Byte 0: x... .... Undefined
;            .1.. .... Sync byte, marks first byte of packet
;            ..L. .... Left button (button 1) status, 1 = depressed
;            ...R .... Right button (button 2) status, 1 = depressed
;            .... YY.. High order bits of Delta-Y (signed 2's complement)
;            .... ..XX High order bits of Delta-X (signed 2's complement)
;
;    Byte 1: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..XX XXXX Low order bits of Delta-X (column)
;
;    Byte 2: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..YY YYYY Low order bits of Delta-Y (row)
;
;    Byte 3: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..M. .... Middle button (button 3) status, 1 = depressed
;            ...T TTTT Device type, 00000 = mouse
;
;    The fourth byte is only returned for three-button serial mice.  Further,
; the fourth byte is only returned when the middle button is pressed or
; released, so either a three button or four button packet must be handled.
;

cseg     segment

         align    4

InterruptHandlerMP proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrMP1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrMP2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         and      al,7fh                  ;
         test     al,sync7Bit             ;
         jz       intrMP3                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrMP1           ;  discarded.
intrMP2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrMP3: mov      di,byteCnt              ; Packets may be either three or
         or       di,di                   ;  four bytes in length.
         jz       intrMP1                 ;
         mov      mEvent[di],al           ; Process all packets when three
         inc      di                      ;  bytes have been received.  If a
         mov      byteCnt,di              ;  fourth byte is received process
         cmp      di,sizPktMP-1           ;  the packet again, but set deltaX
         jc       intrMP1                 ;  and deltaY to zero since they
         jz       intrMP4                 ;  have been seen.  When handling
         mov      byteCnt,0               ;  a three byte packet, the last
         and      mEvent[0],070h          ;  state of the fourth byte of the
         mov      mEvent[1],0             ;  last four-byte packet will be
         mov      mEvent[2],0             ;   used (or zero if none).
intrMP4: test     devStatus,readEnable    ;
         jz       intrMP1                 ; If reporting disabled then do
         mov      ah,mEvent[0]            ;  nothing further for packet
         mov      bl,ah                   ;
         mov      bh,mEvent[3]            ; Put delta-X into dx and delta-Y
         add      bh,bh                   ;  into bx
         and      bx,4030h                ;
         or       bl,bh                   ; Get event flags in cx
         shr      bx,4                    ;  if button pressed and moved
         xor      bh,bh                   ;    0000 0000 00M0 R0L0
         movzx    si,intrMap[bx]          ;  if button pressed only
         mov      al,mEvent[1]            ;    0000 0000 0M0R 0L00
         shl      al,2                    ;  if move only
         shr      ax,2                    ;    0000 0000 0000 0001
         mov      dh,al                   ;  if no button pressed & not moved
         sar      dx,8                    ;    0000 0000 0000 0000
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ; Optionally reverse delta-X
         mov      al,mEvent[2]            ;
         shl      al,2                    ;
         shr      ax,2                    ;
         mov      bh,al                   ;
         sar      bx,8                    ;
         xor      bx,rowRvFlg             ; Optionally reverse delta-Y
         add      bx,rowRvVal             ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ; Optionally exchange the X
         shld     dx,bx,cl                ;  and Y axis
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;
         or       ax,dx                   ; Where L, R and M are 1 if the
         jnz      intrMP5                 ;  corresponding button was pressed
         add      cx,cx                   ;  and 0 otherwise
         cmp      cx,lastEvnt             ;
         jnz      intrMP6                 ;
         jmp      intrMP1                 ;
intrMP5: cmp      cx,1                    ;
         adc      cx,0                    ;
intrMP6: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ; Set up packet addressing
         push     ds                      ;
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
         jmp      intrMP1                 ;

InterruptHandlerMP endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerMI()
;
; Description:
;
;    Handles interrupts for the MP communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common relative event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    The MP communication protocol returns either a 3 or 4 byte packet, and
; has the format...
;
;    Byte 0: x... .... Undefined
;            .1.. .... Sync byte, marks first byte of packet
;            ..L. .... Left button (button 1) status, 1 = depressed
;            ...R .... Right button (button 2) status, 1 = depressed
;            .... YY.. High order bits of Delta-Y (signed 2's complement)
;            .... ..XX High order bits of Delta-X (signed 2's complement)
;
;    Byte 1: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..XX XXXX Low order bits of Delta-X (column)
;
;    Byte 2: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..YY YYYY Low order bits of Delta-Y (row)
;
;    Byte 3: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..M. .... Middle button (button 3) status, 1 = depressed
;            ...T TTTT Device type, 00000 = mouse
;
;    The fourth byte is only returned for three-button serial mice.  Further,
; the fourth byte is only returned when the middle button is pressed or
; released, so either a three button or four button packet must be handled.
;

cseg     segment

         align    4

InterruptHandlerMI proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrMI1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrMI2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         and      al,7fh                  ;
         test     al,sync7Bit             ;
         jz       intrMI3                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrMI1           ;  discarded.
intrMI2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrMI3: mov      di,byteCnt              ; Process packet when all bytes
         or       di,di                   ;  have been received
         jz       intrMI1                 ;
         mov      mEvent[di],al           ;
         inc      di                      ;
         mov      byteCnt,di              ;
         cmp      di,sizPktMI             ;
         jc       intrMI1                 ;
         mov      byteCnt,0               ;
         test     devStatus,readEnable    ;
         jz       intrMI1                 ; If reporting disabled then do
         mov      ah,mEvent[0]            ;  nothing further for packet
         mov      bl,ah                   ;
         and      bx,0030h                ;
         shr      bx,4                    ; Put delta-X into dx and delta-Y
         movzx    si,intrMap[bx]          ;  into bx
         mov      al,byte ptr mEvent[1]   ;
         shl      al,2                    ; Get event flags in cx
         shr      ax,2                    ;  if button pressed and moved
         mov      dh,al                   ;    0000 0000 00M0 R0L0
         sar      dx,8                    ;  if button pressed only
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         mov      al,byte ptr mEvent[2]   ;    0000 0000 0M0R 0L00
         shl      al,2                    ;  if move only
         shr      ax,2                    ;    0000 0000 0000 0001
         mov      bh,al                   ;  if no button pressed & not moved
         sar      bx,8                    ;    0000 0000 0000 0000
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;
         or       ax,dx                   ; Where L, R and M are 1 if the
         jnz      intrMI4                 ;  corresponding button was pressed
         add      cx,cx                   ;  and 0 otherwise
         cmp      cx,lastEvnt             ;
         jnz      intrMI5                 ;
         jmp      intrMI1                 ;
intrMI4: cmp      cx,1                    ;
         adc      cx,0                    ;
intrMI5: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ; Set up packet addressing
         push     ds                      ;
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
         jmp      intrMI1                 ;

InterruptHandlerMI endp

cseg     ends



;
; Interface:
;
;    InterruptHandler5B()
;
; Description:
;
;    Handles interrupts for the 5B communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common relative event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    The 5b communication protocol returns a 5 byte packet, and has the
; format...
;
;    Byte 0: 1000 0... Sync byte, marks first byte of packet
;            .... .L.. Left button   (button 1) status, 0 = depressed
;            .... ..M. Middle button (button 3) status, 0 = depressed
;            .... ...R Right button  (button 2) status, 0 = depressed
;
;    Byte 1: XXXX XXXX First delta-X (column)
;
;    Byte 2: YYYY YYYY First delta-Y (row)
;
;    Byte 3: XXXX XXXX Second delta-X (column)
;
;    Byte 4: YYYY YYYY Second delta-Y (row)
;
;    Bytes 2 and 3 contain the distance that the mouse has moved since the
; last packet, and bytes 4 and 5 contain the distance that the mouse has
; moved since the first byte of the packet.  Therefore, the actual distance
; moved in the sum of the individual distances.
;

cseg     segment

         align    4

InterruptHandler5B proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intr5B1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intr5B6                 ;
         sub      dx,lsr                  ; Read data byte from serial port,
         in       al,dx                   ;  if not valid format for first
         mov      di,byteCnt              ;  byte then ignore it
         or       di,di                   ;
         jnz      intr5B2                 ;
         mov      ah,al                   ;
         and      ah,0f8h                 ;
         cmp      ah,080h                 ;
         jnz      intr5B1                 ;
intr5B2: mov      mEvent[di],al           ; Accumulate packet bytes in
         inc      di                      ;  mEvent.  Incomplete packets
         mov      byteCnt,di              ;  and spurious data will be
         cmp      di,sizPkt5B             ;  discarded.
         jc       intr5B1                 ;
         mov      byteCnt,0               ; If reporting disabled then do
         test     devStatus,readEnable    ;  nothing further for packet
         jz       intr5B1                 ;
         mov      bl,mEvent[0]            ; Get button key press translation
         and      bx,0007h                ;
         movzx    si,intrMap[bx]          ; Put delta-X into dx and delta-Y
         movsx    dx,mEvent[1]            ;  into bx
         movsx    ax,mEvent[3]            ;
         add      dx,ax                   ; Get event flags in cx
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         movsx    bx,mEvent[2]            ;  if button pressed and moved
         movsx    ax,mEvent[4]            ;    0000 0000 00M0 R0L0
         add      bx,ax                   ;  if button pressed only
         xor      bx,rowRvFlg             ;    0000 0000 0M0R 0L00
         add      bx,rowRvVal             ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;  if move only
         or       ax,dx                   ;    0000 0000 0000 0001
         jnz      intr5B4                 ;  if no button pressed & not moved
         add      cx,cx                   ;    0000 0000 0000 0000
         cmp      cx,lastEvnt             ;
         jnz      intr5B5                 ;
         jmp      intr5B1                 ; Where L, R and M are 1 if the
intr5B4: cmp      cx,1                    ;  corresponding button was pressed
         adc      cx,0                    ;  and 0 otherwise
intr5B5: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ;
         push     ds                      ; Set up packet addressing
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
         jmp      intr5B1                 ;
intr5B6: cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts and reenable
         in       al,dx                   ;  mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;

InterruptHandler5B endp

cseg     ends



;
; Interface:
;
;    InterruptHandler3B()
;
; Description:
;
;    Handles interrupts for the 3B communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common relative event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    The 3b communication protocol returns a 3 byte packet, and has the
; format...
;
;    Byte 0: 1000 0... Sync byte, marks first byte of packet
;            .... .L.. Left button   (button 1) status, 0 = depressed
;            .... ..M. Middle button (button 3) status, 0 = depressed
;            .... ...R Right button  (button 2) status, 0 = depressed
;
;    Byte 1: XXXX XXXX First delta-X (column)
;
;    Byte 2: YYYY YYYY First delta-Y (row)
;
; Note: This is an experimental protocol.  It has been tested using a mouse
;       that sends the 5B protocol (ignoring the last two bytes of each
;       packet) but no documentation for this protocol is currently
;       available, nor has a mouse which returns this protocol been found.
;       It is defined merely on the basis in the similarity of names with
;       the 5B protocol.
;

cseg     segment

         align    4

InterruptHandler3B proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intr3B1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intr3B6                 ;
         sub      dx,lsr                  ; Read data byte from serial port,
         in       al,dx                   ;  if not valid format for first
         mov      di,byteCnt              ;  byte then ignore it
         or       di,di                   ;
         jnz      intr3B2                 ;
         mov      ah,al                   ;
         and      ah,0f8h                 ;
         cmp      ah,080h                 ;
         jnz      intr3B1                 ;
intr3B2: mov      mEvent[di],al           ; Accumulate packet bytes in
         inc      di                      ;  mEvent.  Incomplete packets
         mov      byteCnt,di              ;  and spurious data will be
         cmp      di,sizPkt3B             ;  discarded.
         jc       intr3B1                 ;
         mov      byteCnt,0               ; If reporting disabled then do
         test     devStatus,readEnable    ;  nothing further for packet
         jz       intr3B1                 ;
         mov      bl,mEvent[0]            ; Get button key press translation
         and      bx,0007h                ;
         movzx    si,intrMap[bx]          ; Put delta-X into dx and delta-Y
         movsx    dx,mEvent[1]            ;  into bx
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         movsx    bx,mEvent[2]            ;
         xor      bx,rowRvFlg             ; Get event flags in cx
         add      bx,rowRvVal             ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;  if button pressed and moved
         or       ax,dx                   ;    0000 0000 00M0 R0L0
         jnz      intr3B4                 ;  if button pressed only
         add      cx,cx                   ;    0000 0000 0M0R 0L00
         cmp      cx,lastEvnt             ;  if move only
         jnz      intr3B5                 ;    0000 0000 0000 0001
         jmp      intr3B1                 ;  if no button pressed & not moved
intr3B4: cmp      cx,1                    ;    0000 0000 0000 0000
         adc      cx,0                    ;
intr3B5: mov      lastEvnt,cx             ; Where L, R and M are 1 if the
         mov      si,intPacketOff         ;  corresponding button was pressed
         push     ds                      ;  and 0 otherwise
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Set up packet addressing
         mov      es,ax                   ;
         mov      [si].event,cx           ; Put event flags, row movement and
         call     es:[accelrat]           ;  column movement into event packet
         mov      [si].colMov,dx          ; Accelerate/deaccelerate mouse
         mov      [si].rowMov,bx          ;
         mov      ax,eventRelative        ; Go handle relative event,
         call     es:mseDd.protEntry      ;  interrupts are enabled
         pop      ds                      ;  during processing.  Handler
         jmp      intr3B1                 ;  is NOT reentrant!
intr3B6: cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts and reenable
         in       al,dx                   ;  mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;

InterruptHandler3B endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerMM()
;
; Description:
;
;    Handles interrupts for the MM communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common relative event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    The MM communication protocol returns a 3 byte packet, and has the
; format...
;
;    Byte 0: 1... .... Sync byte, marks first byte of packet
;            .P.. .... Out of proximity, digitizer only (0 = in-prox)
;            ..T. .... Tablet identifier, digitizer only, command controlled
;            ...X .... DeltaX sign bit, 0 = negative, 1 = positive
;            .... Y... DeltaY sign bit, 0 = negative, 1 = positive
;            .... .L.. Left button   (button 1) status, 1 = depressed
;            .... ..M. Middle button (button 3) status, 1 = depressed
;            .... ...R Right button  (button 2) status, 1 = depressed
;
;    Byte 1: 0... .... 0 = Non-Sync byte
;            .XXX XXXX First delta-X (column)
;
;    Byte 2: 0... .... 0 = Non-Sync byte
;            .YYY YYYY First delta-Y (row)
;

cseg     segment

         align    4

InterruptHandlerMM proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrMM1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrMM2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         test     al,sync8Bit             ;
         jz       intrMM3                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrMM1           ;  discarded.
intrMM2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrMM3: mov      di,byteCnt              ; Process packet when all bytes
         or       di,di                   ;  have been received
         jz       intrMM1                 ;
         mov      mEvent[di],al           ;
         inc      di                      ;
         mov      byteCnt,di              ;
         cmp      di,sizPktMM             ;
         jc       intrMM1                 ;
         mov      byteCnt,0               ; If reporting disabled then do
         test     devStatus,readEnable    ;  nothing
         jz       intrMM1                 ;
         mov      bl,mEvent[0]            ; Get button key press translation
         and      bx,0007h                ;
         movzx    si,intrMap[bx]          ; Put delta-X into dx and delta-Y
         mov      al,mEvent[0]            ;  into bx
         movzx    dx,mEvent[1]            ;
         bt       ax,4                    ;
         sbb      bx,bx                   ;
         xor      bx,colRvFlg             ; Get event flags in cx
         xor      dx,bx                   ;  if button pressed and moved
         sub      dx,bx                   ;
         movzx    bx,mEvent[2]            ;    0000 0000 00M0 R0L0
         bt       ax,3                    ;  if button pressed only
         sbb      ax,ax                   ;    0000 0000 0M0R 0L00
         xor      ax,rowRvFlg             ;  if move only
         xor      bx,ax                   ;
         sub      bx,ax                   ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;    0000 0000 0000 0001
         or       ax,dx                   ;  if no button pressed & not moved
         jnz      intrMM6                 ;    0000 0000 0000 0000
         add      cx,cx                   ;
         cmp      cx,lastEvnt             ;
         jnz      intrMM7                 ;
         jmp      intrMM1                 ; Where L, R and M are 1 if the
intrMM6: cmp      cx,1                    ;  corresponding button was pressed
         adc      cx,0                    ;  and 0 otherwise
intrMM7: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ;
         push     ds                      ; Set up packet addressing
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
         jmp      intrMM1                 ;

InterruptHandlerMM endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerRE()
;
; Description:
;
;    Handles interrupts for the RE communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common relative event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    The RE communication protocol returns a 5 byte packet, and has the
; format...
;
;    Byte 0: x... .... Undefined
;            .1.. .... Sync byte, marks first byte of packet
;            ..0. ..00 Reserved
;            ...L .... Left button   (button 1) status, 1 = depressed
;            .... M... Middle button (button 3) status, 1 = depressed
;            .... .R.. Right button  (button 2) status, 1 = depressed
;
;    Byte 1: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..XX XXXX Least significant 6 bits of delta-X (column)
;
;    Byte 2: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..XX XXXX Most significant 6 bits of delta-X (column)
;
;    Byte 3: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..YY YYYY Least significant 6 bits of delta-Y (row)
;
;    Byte 4: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..YY YYYY Most significant 6 bits of delta-Y (row)
;

cseg     segment

         align    4

InterruptHandlerRE proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrRE1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrRE2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         and      al,7fh                  ;
         test     al,sync7Bit             ;
         jz       intrRE3                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrRE1           ;  discarded.
intrRE2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrRE3: mov      di,byteCnt              ; Packets are three bytes
         or       di,di                   ;
         jz       intrRE1                 ;
         mov      mEvent[di],al           ; Process all packets when three
         inc      di                      ;  bytes have been received.
         mov      byteCnt,di              ;
         cmp      di,sizPktRE             ;
         jc       intrRE1                 ;
         mov      byteCnt,0               ;
         test     devStatus,readEnable    ; If reporting disabled then do
         jz       intrRE1                 ;  nothing
         mov      bl,mEvent[0]            ;
         shr      bl,2                    ; Get button key press translation
         and      bx,0007h                ;
         movzx    si,intrMap[bx]          ;
         mov      dl,mEvent[1]            ;
         add      dl,dl                   ; Put delta-X into dx and delta-Y
         add      dl,dl                   ;  int bx
         mov      dh,mEvent[2]            ;
         add      dh,dh                   ;
         add      dh,dh                   ;
         sar      dh,2                    ;
         sar      dx,2                    ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         mov      bl,mEvent[3]            ; Get event flags in cx
         add      bl,bl                   ;  if button pressed and moved
         add      bl,bl                   ;    0000 0000 00M0 R0L0
         mov      bh,mEvent[4]            ;  if button pressed only
         add      bh,bh                   ;    0000 0000 0M0R 0L00
         add      bh,bh                   ;  if move only
         sar      bh,2                    ;    0000 0000 0000 0001
         sar      bx,2                    ;  if no button pressed & not moved
         xor      bx,rowRvFlg             ;    0000 0000 0000 0000
         add      bx,rowRvVal             ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;
         or       ax,dx                   ; Where L, R and M are 1 if the
         jnz      intrRE4                 ;  corresponding button was pressed
         add      cx,cx                   ;  and 0 otherwise
         cmp      cx,lastEvnt             ;
         jnz      intrRE5                 ;
         jmp      intrRE1                 ;
intrRE4: cmp      cx,1                    ;
         adc      cx,0                    ;
intrRE5: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ; Set up packet addressing
         push     ds                      ;
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
         jmp      intrRE1                 ;

InterruptHandlerRE endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerHR()
;
; Description:
;
;    Handles interrupts for the HR communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common relative event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    If the cursor is out of proximity, or if either the row or column is
; negative or out of the allowed range then an "out of range" flag is set
; and the next "in range" data packet will be ignored except for resetting
; the last known cursor position.  If the cursor has moved 1000 or more
; points in either the X or Y direction then the data packet is treated as
; if the "out of range" flag were set (this provision is for tablets which
; do NOT generate "out of proximity" data points.  A "relative" protocol
; allows the mouse to "drift" and so it must be necessary to pick the cursor
; up and move it over without affecting the mouse pointer.
;
;    The HR communication protocol returns a 6 byte packet, and has the
; format...
;
;    Byte 0: 1... .... Sync byte, marks first byte of packet
;            .CCC CC.. Cursor key code (see below)
;            .... ..XX X position (bits 14..15)
;
;    Byte 1: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 7..13)
;
;    Byte 2: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 0..6)
;
;    Byte 3: 0... .... 0 = Non-Sync byte
;            .0.. .... Reserved
;            ..P. .... Cursor in active area of digitizer (P = 0)
;            ...X X... X position (bits 16..17)
;            .... .YYY Y position (bits 14..16)
;
;    Byte 4: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 7..13)
;
;    Byte 5: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 0..6)
;
; Note: OS/2 can only handle 16-bit positions.  Therefore, the digitizer
;       must be programmed so that the number of horizontal and vertical
;       positions does not exceed 65535.  This interrupt handler will
;       ignore the position bits which do not fit into a word.  Only the
;       largest digitizers using the higher resolutions can exceed this
;       limitation.
;

cseg     segment

         align    4

InterruptHandlerHR proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrHR1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrHR2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         test     al,sync8Bit             ;
         jz       intrHR4                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrHR1           ;  discarded.
intrHR2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrHR3: mov      outOfRng,1              ; Set cursor out of range flag
         jmp      intrHR1                 ;  to allow repositioning cursor
intrHR4: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrHR1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktHR             ;
         jc       intrHR1                 ;
         mov      byteCnt,0               ;
         test     mEvent[3],34h           ; If cursor out of proximity then
         jnz      intrHR3                 ;  set flag to reset cursor position
         mov      bl,mEvent[0]            ;
         shr      bl,2                    ; Get button key press translation
         and      bx,001fh                ;
         movzx    si,intrMap[bx]          ;
         mov      dh,mEvent[0]            ;
         mov      dl,mEvent[1]            ; Put X position in dx
         add      dl,dl                   ;
         shl      dx,6                    ;
         or       dl,mEvent[2]            ;
         cmp      dx,colMax               ; If X position is outside of
         jnc      intrHR3                 ;  [colMin..colMax) then cursor
         cmp      dx,colMin               ;  is out of range
         jc       intrHR3                 ;
         sub      dx,colMin               ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         xchg     dx,lastCol              ;
         mov      bh,mEvent[3]            ;
         mov      bl,mEvent[4]            ; Put Y position in bx (eventually)
         add      bl,bl                   ;
         shl      bx,6                    ;
         or       bl,mEvent[5]            ;
         cmp      bx,rowMax               ; If Y position is outside of
         jnc      intrHR3                 ;  [rowMin..rowMax) then cursor
         cmp      bx,rowMin               ;  is out of range
         jc       intrHR3                 ;
         sub      bx,rowMin               ;
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ;
         xchg     bx,lastRow              ;
         xor      al,al                   ; If this was first good data point
         xchg     al,outOfRng             ;  then keep position and ignore
         or       al,al                   ;  remainder of packet
         jnz      intrHR1                 ;
         test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrHR1                 ;  not active then do nothing
         sub      dx,lastCol              ;
         cmp      dx,1000                 ; Get relative X motion in dx, if
         jge      intrHR1                 ;  moved at least 1000 then treat
         neg      dx                      ;  cursor as out of range
         cmp      dx,1000                 ;
         jge      intrHR1                 ;
         sub      bx,lastRow              ;
         cmp      bx,1000                 ; Get relative Y motion in bx, if
         jge      intrHR1                 ;  moved more than 1000 then treat
         neg      bx                      ;  cursor as out of range
         cmp      bx,1000                 ;
         jge      intrHR1                 ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;
         or       ax,dx                   ; If movement, always report packet
         jnz      intrHR5                 ;
         add      cx,cx                   ;
         cmp      cx,lastEvnt             ; If no movement and same buttons
         jnz      intrHR6                 ;  pressed as before ignore packet
         jmp      intrHR1                 ;
intrHR5: cmp      cx,1                    ; Get event flags in cx
         adc      cx,0                    ;
intrHR6: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ; Set up packet addressing
         push     ds                      ;
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
         jmp      intrHR1                 ;

InterruptHandlerHR endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerMR()
;
; Description:
;
;    Handles interrupts for the MR communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common relative event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    If the cursor is out of proximity, or if either the row or column is
; negative or out of the allowed range then an "out of range" flag is set
; and the next "in range" data packet will be ignored except for resetting
; the last known cursor position.  If the cursor has moved 500 or more
; points in either the X or Y direction then the data packet is treated as
; if the "out of range" flag were set (this provision is for tablets which
; do NOT generate "out of proximity" data points.  A "relative" protocol
; allows the mouse to "drift" and so it must be necessary to pick the cursor
; up and move it over without affecting the mouse pointer.
;
;    The MR communication protocol returns a 5 byte packet, and has the
; format...
;
;    Byte 0: 1... .... Sync byte, marks first byte of packet
;            .P.. .... Out of proximity (0 = in-prox)
;            ..T. .... Tablet identifier, command controlled
;            ...X .... Sign bit, 0 = negative, 1 = positive
;            .... Y... Sign bit, 0 = negative, 1 = positive
;            .... .R.. Left button   (button 3) status, 1 = depressed
;            .... ..L. Middle button (button 1) status, 1 = depressed
;            .... ...M Right button  (button 2) status, 1 = depressed
;
;    Byte 1: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 0..6)
;
;    Byte 2: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 7..13)
;
;    Byte 3: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 0..6)
;
;    Byte 4: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 7..13)
;

cseg     segment

         align    4

InterruptHandlerMR proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrMR1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrMR2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         test     al,sync8Bit             ;
         jz       intrMR4                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrMR1           ;  discarded.
intrMR2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrMR3: mov      outOfRng,1              ; Set cursor out of range flag
         jmp      intrMR1                 ;  to allow repositioning cursor
intrMR4: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrMR1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktMR             ;
         jc       intrMR1                 ;
         mov      byteCnt,0               ;
         xor      mEvent[0],18h           ;
         test     mEvent[0],58h           ; If cursor out of proximity then
         jnz      intrMR3                 ;  set flag to reset cursor position
         mov      bl,mEvent[0]            ;
         and      bx,0007h                ; Get button key press translation
         movzx    si,intrMap[bx]          ;
         xor      dx,dx                   ;
         mov      dh,mEvent[2]            ; Put X position in dx
         shr      dx,1                    ;
         or       dl,mEvent[1]            ;
         cmp      dx,colMax               ; If X position is outside of
         jnc      intrMR3                 ;  [colMin..colMax) then cursor
         cmp      dx,colMin               ;  is out of range
         jc       intrMR3                 ;
         sub      dx,colMin               ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         xchg     dx,lastCol              ;
         xor      bx,bx                   ;
         mov      bh,mEvent[4]            ; Put Y position in bx
         shr      bx,1                    ;
         or       bl,mEvent[3]            ;
         cmp      bx,rowMax               ; If Y position is outside of
         jnc      intrMR3                 ;  [rowMin..rowMax) then cursor
         cmp      bx,rowMin               ;  is out of range
         jc       intrMR3                 ;
         sub      bx,rowMin               ;
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ;
         xchg     bx,lastRow              ;
         xor      al,al                   ; If this was first good data point
         xchg     al,outOfRng             ;  then keep position and ignore
         or       al,al                   ;  remainder of packet
         jnz      intrMR1                 ;
         test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrMR1                 ;  not active then do nothing
         sub      dx,lastCol              ;
         cmp      dx,500                  ; Get relative X motion in dx, if
         jge      intrMR1                 ;  moved at least 500 then treat
         neg      dx                      ;  cursor as out of range
         cmp      dx,500                  ;
         jge      intrMR1                 ;
         sub      bx,lastRow              ;
         cmp      bx,500                  ; Get relative Y motion in bx, if
         jge      intrMR1                 ;  moved more than 500 then treat
         neg      bx                      ;  cursor as out of range
         cmp      bx,500                  ;
         jge      intrMR1                 ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;
         or       ax,dx                   ; If movement, always report packet
         jnz      intrMR5                 ;
         add      cx,cx                   ;
         cmp      cx,lastEvnt             ; If no movement and same buttons
         jnz      intrMR6                 ;  pressed as before ignore packet
         jmp      intrMR1                 ;
intrMR5: cmp      cx,1                    ; Get event flags in cx
         adc      cx,0                    ;
intrMR6: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ; Set up packet addressing
         push     ds                      ;
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
         jmp      intrMR1                 ;

InterruptHandlerMR endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerTR()
;
; Description:
;
;    Handles interrupts for the TR communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common relative event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    If the cursor is out of proximity then an "out of range" flag is set
; and the next "in range" data packet will be ignored except for resetting
; the last known cursor position.
;
;    The TR communication protocol returns a 5 byte packet, and has the
; format...
;
;    Byte 0: 1... .... Sync byte, marks first byte of packet
;            .P.. .... Out of proximity (1 = in-prox)
;            ..xx xx.. Undefined
;            .... ..R. Middle button (button 1) status, 1 = depressed
;            .... ...L Right button  (button 2) status, 1 = depressed
;
;    Byte 1: 0... .... 0 = Non-Sync byte
;            .XXX .... X position (bits 0..2
;            .... xxxx Undefined
;
;    Byte 2: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 3..9)
;
;    Byte 3: 0... .... 0 = Non-Sync byte
;            .YYY .... Y position (bits 0..2)
;            .... xxxx Undefined
;
;    Byte 4: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 3..9)
;

cseg     segment

         align    4

InterruptHandlerTR proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrTR1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrTR2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         test     al,sync8Bit             ;
         jz       intrTR4                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrTR1           ;  discarded.
intrTR2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrTR3: mov      outOfRng,1              ; Set cursor out of range flag
         jmp      intrTR1                 ;  to allow repositioning cursor
intrTR4: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrTR1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktTR             ;
         jc       intrTR1                 ;
         mov      byteCnt,0               ;
         mov      bl,mEvent[0]            ;
         and      bx,0003h                ; Get button key press translation
         movzx    si,intrMap[bx]          ;
         test     mEvent[0],40h           ; If cursor out of proximity then
         jnz      intrTR6                 ;  set flag to reset cursor position
intrTR5: cmp      si,lastEvnt             ;
         jz       intrTR3                 ;
         mov      dx,lastCol              ;
         mov      bx,lastRow              ;
         jmp      short intrTR7           ;
intrTR6: mov      dl,mEvent[1]            ;
         add      dl,dl                   ; Put X position in dx
         mov      dh,mEvent[2]            ;
         shr      dx,5                    ;
         cmp      dx,colMax               ; If X position is outside of
         jnc      intrTR5                 ;  [colMin..colMax) then cursor
         cmp      dx,colMin               ;  is out of range
         jc       intrTR5                 ;
         sub      dx,colMin               ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         mov      bl,mEvent[3]            ;
         add      bl,bl                   ; Put Y position in bx
         mov      bh,mEvent[4]            ;
         shr      bx,5                    ;
         cmp      bx,rowMax               ; If Y position is outside of
         jnc      intrTR5                 ;  [rowMin..rowMax) then cursor
         cmp      bx,rowMin               ;  is out of range
         jc       intrTR5                 ;
         sub      bx,rowMin               ;
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ;
         xchg     dx,lastCol              ;
         xchg     bx,lastRow              ;
         xor      al,al                   ; If this was first good data point
         xchg     al,outOfRng             ;  then keep position and ignore
         or       al,al                   ;  remainder of packet
         jnz      intrTR1                 ;
intrTR7: test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrTR1                 ;  not active then do nothing
         sub      dx,lastCol              ;
         sub      bx,lastRow              ;
         mov      ax,bx                   ;
         or       ax,dx                   ; If movement, always report packet
         jnz      intrTR8                 ;
         mov      cx,si                   ;
         add      cx,cx                   ;
         cmp      si,lastEvnt             ; If no movement and same buttons
         jnz      intrTR9                 ;  pressed as before ignore packet
         jmp      intrTR1                 ;
intrTR8: mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         cmp      cx,1                    ; Get event flags in cx
         adc      cx,0                    ;
intrTR9: mov      lastEvnt,si             ;
         mov      si,intPacketOff         ; Set up packet addressing
         push     ds                      ;
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
         jmp      intrTR1                 ;

InterruptHandlerTR endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerUR()
;
; Description:
;
;    Handles interrupts for the UR communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common relative event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    If the cursor is out of proximity, or if either the row or column is
; negative or out of the allowed range then an "out of range" flag is set
; and the next "in range" data packet will be ignored except for resetting
; the last known cursor position.  If the cursor has moved 500 or more
; points in either the X or Y direction then the data packet is treated as
; if the "out of range" flag were set (this provision is for tablets which
; do NOT generate "out of proximity" data points.  A "relative" protocol
; allows the mouse to "drift" and so it must be necessary to pick the cursor
; up and move it over without affecting the mouse pointer.
;
;    The UR communication protocol returns an 8 byte packet, and has the
; format...
;
;    Byte 0: x... .... Undefined
;            .1.. .... Sync byte, marks first byte of packet
;            ..00 00.. Reserved
;            .... ..T. Tablet identifier, command controlled
;            .... ...P Out of proximity (0 = in-prox)
;
;    Byte 1: x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..0. .... Reserved
;            ...B BBBB Button flags
;
;    Byte 2  x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..XX XXXX X position (bits 0..5)
;
;    Byte 3: x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..XX XXXX X position (bits 6..11)
;
;    Byte 4: x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..0. .... Reserved
;            ...S .... Sign bit for X
;            .... XXXX X position (bits 12..15)
;
;    Byte 5: x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..YY YYYY Y position (bits 0..5)
;
;    Byte 6: x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..YY YYYY Y position (bits 6..11)
;
;    Byte 7: x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..0. .... Reserved
;            ...S .... Sign bit for Y
;            .... YYYY Y position (bits 12..15)
;

cseg     segment

         align    4

InterruptHandlerUR proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrUR1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrUR2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         and      al,7fh                  ;
         test     al,sync7Bit             ;
         jz       intrUR4                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrUR1           ;  discarded.
intrUR2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrUR3: mov      outOfRng,1              ; Set cursor out of range flag
         jmp      intrUR1                 ;  to allow repositioning cursor
intrUR4: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrUR1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktUR             ;
         jc       intrUR1                 ;
         mov      byteCnt,0               ;
         test     mEvent[0],01h           ;
         jnz      intrUR3                 ; If cursor out of proximity or
         test     mEvent[4],10h           ;  position is negative then set
         jnz      intrUR3                 ;  flag to reset cursor position
         test     mEvent[7],10h           ;
         jnz      intrUR3                 ;
         mov      bl,mEvent[1]            ;
         and      bx,000fh                ; Get button key press translation
         movzx    si,intrMap[bx]          ;
         xor      dx,dx                   ;
         mov      dx,word ptr mEvent[3]   ;
         shl      dl,2                    ; Put X position in dx
         shl      dx,4                    ;
         or       dl,mEvent[2]            ;
         cmp      dx,colMax               ; If X position is outside of
         jnc      intrUR3                 ;  [colMin..colMax) then cursor
         cmp      dx,colMin               ;  is out of range
         jc       intrUR3                 ;
         sub      dx,colMin               ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         xchg     dx,lastCol              ;
         xor      bx,bx                   ;
         mov      bx,word ptr mEvent[6]   ;
         shl      bl,2                    ; Put Y position in bx
         shl      bx,4                    ;
         or       bl,mEvent[5]            ;
         cmp      bx,rowMax               ; If Y position is outside of
         jnc      intrUR3                 ;  [rowMin..rowMax) then cursor
         cmp      bx,rowMin               ;  is out of range
         jc       intrUR3                 ;
         sub      bx,rowMin               ;
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ;
         xchg     bx,lastRow              ;
         xor      al,al                   ; If this was first good data point
         xchg     al,outOfRng             ;  then keep position and ignore
         or       al,al                   ;  remainder of packet
         jnz      intrUR1                 ;
         test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrUR1                 ;  not active then do nothing
         sub      dx,lastCol              ;
         cmp      dx,1000                 ; Get relative X motion in dx, if
         jge      intrUR1                 ;  moved at least 1000 then treat
         neg      dx                      ;  cursor as out of range
         cmp      dx,1000                 ;
         jge      intrUR1                 ;
         sub      bx,lastRow              ;
         cmp      bx,1000                 ; Get relative Y motion in bx, if
         jge      intrUR1                 ;  moved more than 1000 then treat
         neg      bx                      ;  cursor as out of range
         cmp      bx,1000                 ;
         jge      intrUR1                 ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;
         or       ax,dx                   ; If movement, always report packet
         jnz      intrUR5                 ;
         add      cx,cx                   ;
         cmp      cx,lastEvnt             ; If no movement and same buttons
         jnz      intrUR6                 ;  pressed as before ignore packet
         jmp      intrUR1                 ;
intrUR5: cmp      cx,1                    ; Get event flags in cx
         adc      cx,0                    ;
intrUR6: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ; Set up packet addressing
         push     ds                      ;
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
         jmp      intrUR1                 ;

InterruptHandlerUR endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerBR()
;
; Description:
;
;    Handles interrupts for the BR communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common relative event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    If the cursor is out of proximity, or if either the row or column is
; negative or out of the allowed range then an "out of range" flag is set
; and the next "in range" data packet will be ignored except for resetting
; the last known cursor position.  If the cursor has moved 500 or more
; points in either the X or Y direction then the data packet is treated as
; if the "out of range" flag were set (this provision is for tablets which
; do NOT generate "out of proximity" data points.  A "relative" protocol
; allows the mouse to "drift" and so it must be necessary to pick the cursor
; up and move it over without affecting the mouse pointer.
;
;    The BR communication protocol returns a 5 byte packet, and has the
; format...
;
;    Byte 0: x... .... Undefined
;            .1.. .... Sync byte, marks first byte of packet
;            ..1. .... Button 4 status, 1 = depressed
;            ...1 .... Button 3 status, 1 = depressed
;            .... 1... Button 2 status, 1 = depressed
;            .... .1.. Button 1 status, 1 = depressed
;            .... ..0. Always 0
;            .... ...0 Bit Pad One - always 0
;                      Bit Pad Two - proximity, 0 = in-proximity
;
;    Byte 1: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..XX XXXX X position (bits 0..5)
;
;    Byte 2: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..XX XXXX X position (bits 6..11)
;
;    Byte 3: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..YY YYYY Y position (bits 0..5)
;
;    Byte 4: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..YY YYYY Y position (bits 6..11)
;

cseg     segment

         align    4

InterruptHandlerBR proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrBR1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrBR2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         test     al,sync7Bit             ;
         jz       intrBR4                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrBR1           ;  discarded.
intrBR2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrBR3: mov      outOfRng,1              ; Set cursor out of range flag
         jmp      intrBR1                 ;  to allow repositioning cursor
intrBR4: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrBR1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktBR             ;
         jc       intrBR1                 ;
         mov      byteCnt,0               ;
         mov      bl,mEvent[0]            ;
         test     bl,01h                  ;
         jnz      intrBR3                 ;
         shr      bx,2                    ;
         and      bx,000fh                ; Get button key press translation
         movzx    si,intrMap[bx]          ;
         xor      dx,dx                   ;
         mov      dh,mEvent[2]            ; Put X position in dx
         shr      dx,2                    ;
         or       dl,mEvent[1]            ;
         cmp      dx,colMax               ; If X position is outside of
         jnc      intrBR3                 ;  [colMin..colMax) then cursor
         cmp      dx,colMin               ;  is out of range
         jc       intrBR3                 ;
         sub      dx,colMin               ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         xchg     dx,lastCol              ;
         xor      bx,bx                   ;
         mov      bh,mEvent[4]            ; Put Y position in bx
         shr      bx,2                    ;
         or       bl,mEvent[3]            ;
         cmp      bx,rowMax               ; If Y position is outside of
         jnc      intrBR3                 ;  [rowMin..rowMax) then cursor
         cmp      bx,rowMin               ;  is out of range
         jc       intrBR3                 ;
         sub      bx,rowMin               ;
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ;
         xchg     bx,lastRow              ;
         xor      al,al                   ; If this was first good data point
         xchg     al,outOfRng             ;  then keep position and ignore
         or       al,al                   ;  remainder of packet
         jnz      intrBR1                 ;
         test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrBR1                 ;  not active then do nothing
         sub      dx,lastCol              ; Get relative X motion in dx
         sub      bx,lastRow              ; Get relative Y motion in bx
         mov      cx,flipFlag             ;
         mov      ax,dx                   ; If X and Y are to interchanged
         shld     dx,bx,cl                ;  then exchange bx and dx
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;
         or       ax,dx                   ; If movement, always report packet
         jnz      intrBR5                 ;
         add      cx,cx                   ;
         cmp      cx,lastEvnt             ; If no movement and same buttons
         jnz      intrBR6                 ;  pressed as before ignore packet
         jmp      intrBR1                 ;
intrBR5: cmp      cx,1                    ; Get event flags in cx
         adc      cx,0                    ;
intrBR6: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ; Set up packet addressing
         push     ds                      ;
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
         jmp      intrBR1                 ;

InterruptHandlerBR endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerCR()
;
; Description:
;
;    Handles interrupts for the CR communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common relative event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    If the cursor is out of proximity, or if either the row or column is
; negative or out of the allowed range then an "out of range" flag is set
; and the next "in range" data packet will be ignored except for resetting
; the last known cursor position.  If the cursor has moved 500 or more
; points in either the X or Y direction then the data packet is treated as
; if the "out of range" flag were set (this provision is for tablets which
; do NOT generate "out of proximity" data points.  A "relative" protocol
; allows the mouse to "drift" and so it must be necessary to pick the cursor
; up and move it over without affecting the mouse pointer.
;
;    The CR communication protocol returns a 5 byte packet, and has the
; format...
;
;    Byte 0: 1... .... Sync byte, marks first byte of packet
;            .0.. .... Proximity, 0 = in-proximity
;            ..0. .... Always 0
;            ...1 .... Always 1
;            .... 1... Always 1
;            .... .FFF Button flags
;
;    Byte 1: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 0..6)
;
;    Byte 2: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 7..13)
;
;    Byte 3: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 0..6)
;
;    Byte 4: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 7..13)
;

cseg     segment

         align    4

InterruptHandlerCR proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrCR1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrCR2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         test     al,sync8Bit             ;
         jz       intrCR4                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrCR1           ;  discarded.
intrCR2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrCR3: mov      outOfRng,1              ; Set cursor out of range flag
         jmp      intrCR1                 ;  to allow repositioning cursor
intrCR4: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrCR1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktCR             ;
         jc       intrCR1                 ;
         mov      byteCnt,0               ;
         mov      bl,mEvent[0]            ;
         test     bl,40h                  ;
         jnz      intrCR3                 ;
         and      bx,0007h                ; Get button key press translation
         movzx    si,intrMap[bx]          ;
         xor      dx,dx                   ;
         mov      dh,mEvent[2]            ; Put X position in dx
         shr      dx,1                    ;
         or       dl,mEvent[1]            ;
         cmp      dx,colMax               ; If X position is outside of
         jnc      intrCR3                 ;  [colMin..colMax) then cursor
         cmp      dx,colMin               ;  is out of range
         jc       intrCR3                 ;
         sub      dx,colMin               ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         xchg     dx,lastCol              ;
         xor      bx,bx                   ;
         mov      bh,mEvent[4]            ; Put Y position in bx
         shr      bx,1                    ;
         or       bl,mEvent[3]            ;
         cmp      bx,rowMax               ; If Y position is outside of
         jnc      intrCR3                 ;  [rowMin..rowMax) then cursor
         cmp      bx,rowMin               ;  is out of range
         jc       intrCR3                 ;
         sub      bx,rowMin               ;
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ;
         xchg     bx,lastRow              ;
         xor      al,al                   ; If this was first good data point
         xchg     al,outOfRng             ;  then keep position and ignore
         or       al,al                   ;  remainder of packet
         jnz      intrCR1                 ;
         test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrCR1                 ;  not active then do nothing
         sub      dx,lastCol              ; Get relative X motion in dx
         sub      bx,lastRow              ; Get relative Y motion in bx
         mov      cx,flipFlag             ;
         mov      ax,dx                   ; If X and Y are to interchanged
         shld     dx,bx,cl                ;  then exchange bx and dx
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;
         or       ax,dx                   ; If movement, always report packet
         jnz      intrCR5                 ;
         add      cx,cx                   ;
         cmp      cx,lastEvnt             ; If no movement and same buttons
         jnz      intrCR6                 ;  pressed as before ignore packet
         jmp      intrCR1                 ;
intrCR5: cmp      cx,1                    ; Get event flags in cx
         adc      cx,0                    ;
intrCR6: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ; Set up packet addressing
         push     ds                      ;
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
         jmp      intrCR1                 ;

InterruptHandlerCR endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerFR()
;
; Description:
;
;    Handles interrupts for the FR communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common relative event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    If the cursor is out of proximity, or if either the row or column is
; negative or out of the allowed range then an "out of range" flag is set
; and the next "in range" data packet will be ignored except for resetting
; the last known cursor position.  If the cursor has moved 500 or more
; points in either the X or Y direction then the data packet is treated as
; if the "out of range" flag were set (this provision is for tablets which
; do NOT generate "out of proximity" data points.  A "relative" protocol
; allows the mouse to "drift" and so it must be necessary to pick the cursor
; up and move it over without affecting the mouse pointer.
;
;    The FR communication protocol returns a 5 byte packet, and has the
; format...
;
;    Byte 0: 1... .... Sync byte, marks first byte of packet
;            .1.. .... Always 1
;            ..R. .... Left button   (button 3) status, 1 = depressed
;            ...L .... Middle button (button 1) status, 1 = depressed
;            .... M... Middle button (button 2) status, 1 = depressed
;            .... .000 Always 0
;
;    Byte 1: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 0..6)
;
;    Byte 2: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 7..13)
;
;    Byte 3: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 0..6)
;
;    Byte 4: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 7..13)
;

cseg     segment

         align    4

InterruptHandlerFR proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrFR1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrFR2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         test     al,sync8Bit             ;
         jz       intrFR4                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrFR1           ;  discarded.
intrFR2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrFR3: mov      outOfRng,1              ; Set cursor out of range flag
         jmp      intrFR1                 ;  to allow repositioning cursor
intrFR4: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrFR1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktFR             ;
         jc       intrFR1                 ;
         mov      byteCnt,0               ;
         mov      bl,mEvent[0]            ;
         shr      bx,3                    ;
         and      bx,0007h                ; Get button key press translation
         movzx    si,intrMap[bx]          ;
         xor      dx,dx                   ;
         mov      dh,mEvent[2]            ; Put X position in dx
         shr      dx,1                    ;
         or       dl,mEvent[1]            ;
         cmp      dx,colMax               ; If X position is outside of
         jnc      intrFR3                 ;  [colMin..colMax) then cursor
         cmp      dx,colMin               ;  is out of range
         jc       intrFR3                 ;
         sub      dx,colMin               ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         xchg     dx,lastCol              ;
         xor      bx,bx                   ;
         mov      bh,mEvent[4]            ; Put Y position in bx (eventually)
         shr      bx,1                    ;
         or       bl,mEvent[3]            ;
         cmp      bx,rowMax               ; If Y position is outside of
         jnc      intrFR3                 ;  [rowMin..rowMax) then cursor
         cmp      bx,rowMin               ;  is out of range
         jc       intrFR3                 ;
         sub      bx,rowMin               ;
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ;
         xchg     bx,lastRow              ;
         xor      al,al                   ; If this was first good data point
         xchg     al,outOfRng             ;  then keep position and ignore
         or       al,al                   ;  remainder of packet
         jnz      intrFR1                 ;
         test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrFR1                 ;  not active then do nothing
         sub      dx,lastCol              ; Get relative X motion in dx
         sub      bx,lastRow              ; Get relative Y motion in bx
         mov      cx,flipFlag             ;
         mov      ax,dx                   ; If X and Y are to interchanged
         shld     dx,bx,cl                ;  then exchange bx and dx
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         mov      ax,bx                   ;
         or       ax,dx                   ; If movement, always report packet
         jnz      intrFR5                 ;
         add      cx,cx                   ;
         cmp      cx,lastEvnt             ; If no movement and same buttons
         jnz      intrFR6                 ;  pressed as before ignore packet
         jmp      intrFR1                 ;
intrFR5: cmp      cx,1                    ; Get event flags in cx
         adc      cx,0                    ;
intrFR6: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ; Set up packet addressing
         push     ds                      ;
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         call     es:[accelrat]           ; Accelerate/deaccelerate mouse
         mov      [si].colMov,dx          ;
         mov      [si].rowMov,bx          ; Go handle relative event,
         mov      ax,eventRelative        ;  interrupts are enabled
         call     es:mseDd.protEntry      ;  during processing.  Handler
         pop      ds                      ;  is NOT reentrant!
         jmp      intrFR1                 ;

InterruptHandlerFR endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerHA()
;
; Description:
;
;    Handles interrupts for the HA communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common absolute event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    The HA communication protocol returns a 6 byte packet, and has the
; format...
;
;    Byte 0: 1... .... Sync byte, marks first byte of packet
;            .CCC CC.. Cursor key code (see below)
;            .... ..XX X position (bits 14..15)
;
;    Byte 1: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 7..13)
;
;    Byte 2: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 0..6)
;
;    Byte 3: 0... .... 0 = Non-Sync byte
;            .0.. .... Reserved
;            ..P. .... Cursor in active area of digitizer (P = 0)
;            ...X X... X position (bits 16..17)
;            .... .YYY Y position (bits 14..16)
;
;    Byte 4: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 7..13)
;
;    Byte 5: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 0..6)
;
; The cursor key code is coded differently for 4 button cursors and 16 button
; cursors.  Since OS/2 only supports three buttons, the extra buttons are
; mapped into chord button presses.  The 16 button cursors only report a
; single button pressed at a time, so this is the ONLY way to obtain chord
; button presses with those cursors.  The 4 button cursor behaves as
; expected.  The key cursor codes are...
;
;    00000 - No buttons pressed
;
;    0.... - Four button cursor in use
;
;    ....1 - Button 0 pressed
;    ...1. - Button 1 pressed
;    ..1.. - Button 2 pressed
;    .1... - Button 3 pressed - we will treat this the same as ...11
;    ..1.1 - Button 4 pressed - the same as pressing buttons 0 and 2
;    ..11. - Button 5 pressed - the same as pressing buttons 1 and 2
;
;    1.... - Sixteen button cursor in use
;    .0000 - Button  0 pressed
;    .0001 - Button  1 pressed
;    .0010 - Button  2 pressed
;    .0011 - Button  3 pressed
;    .0100 - Button  4 pressed
;    .0101 - Button  5 pressed
;    .0110 - Button  6 pressed
;    .0111 - Button  7 pressed
;    .1000 - Button  8 pressed
;    .1001 - Button  9 pressed
;    .1010 - Button 10 pressed
;    .1011 - Button 11 pressed
;    .1100 - Button 12 pressed
;    .1101 - Button 13 pressed
;    .1110 - Button 14 pressed
;    .1111 - Button 15 pressed
;
; Note: OS/2 can only handle 16-bit positions.  Therefore, the digitizer
;       must be programmed so that the number of horizontal and vertical
;       positions does not exceed 65535.  This interrupt handler will
;       ignore the position bits which do not fit into a word.  Only the
;       largest digitizers using the higher resolutions can exceed this
;       limitation.
;

cseg     segment

         align    4

InterruptHandlerHA proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrHA1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrHA2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         test     al,sync8Bit             ;
         jz       intrHA3                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrHA1           ;  discarded.
intrHA2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrHA3: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrHA1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktHA             ;
         jc       intrHA1                 ;
         mov      byteCnt,0               ;
         test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrHA1                 ;  not active then do nothing
         mov      bl,mEvent[0]            ;
         shr      bl,2                    ; Get button key press translation
         and      bx,001fh                ;
         movzx    si,intrMap[bx]          ;
         xor      dx,dx                   ;
         test     mEvent[3],10h           ; Put X position in dx, if less
         jnz      intrHA4                 ;  than zero or greater than
         mov      dh,mEvent[0]            ;  maximum position then it must
         mov      dl,mEvent[1]            ;  be in the margin so pin value
         add      dl,dl                   ;  at the border position
         shl      dx,6                    ;
         or       dl,mEvent[2]            ;
         cmp      dx,colMax               ;
         jc       intrHA4                 ;
         mov      dx,colMax               ;
         dec      dx                      ;
         jmp      short intrHA5           ;
intrHA4: cmp      dx,colMin               ;
         jnc      intrHA5                 ;
         mov      dx,colMin               ;
intrHA5: sub      dx,colMin               ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         xor      bx,bx                   ;
         test     mEvent[3],04h           ; Put Y position in bx, if less
         jnz      intrHA6                 ;  than zero or greater than
         mov      bh,mEvent[3]            ;  maximum position then it must
         mov      bl,mEvent[4]            ;  be in the margin so pin value
         add      bl,bl                   ;  at the border position
         shl      bx,6                    ;
         or       bl,mEvent[5]            ;
         cmp      bx,rowMax               ;
         jc       intrHA6                 ;
         mov      bx,rowMax               ;
         dec      bx                      ;
         jmp      short intrHA7           ;
intrHA6: cmp      bx,rowMin               ;
         jnc      intrHA7                 ; Get event flags in cx
         mov      bx,rowMin               ;  if button pressed and moved
intrHA7: sub      bx,rowMin               ;    0000 0000 00M0 R0L0
         xor      bx,rowRvFlg             ;  if button pressed only
         add      bx,rowRvVal             ;    0000 0000 0M0R 0L00
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         cmp      bx,lastRow              ;  if move only
         jnz      intrHA8                 ;    0000 0000 0000 0001
         cmp      dx,lastCol              ;  if no button pressed & not moved
         jnz      intrHA8                 ;    0000 0000 0000 0000
         add      cx,cx                   ; Where L, R and M are 1 if the
         cmp      cx,lastEvnt             ;  corresponding button was pressed
         jnz      intrHA9                 ;  and 0 otherwise
         jmp      intrHA1                 ;
intrHA8: cmp      cx,1                    ;
         adc      cx,0                    ;
         mov      lastRow,bx              ;
         mov      lastCol,dx              ;
intrHA9: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ;
         push     ds                      ; Set up packet addressing
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         mov      [si].rowPos,bx          ;
         mov      [si].colPos,dx          ; Go handle absolute event,
         mov      bx,es:rowRange          ;  interrupts are enabled
         mov      dx,es:colRange          ;  during processing.  Handler
         mov      cx,es:flipFlag          ;  is NOT reentrant!
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      [si].rowSize,bx         ;
         mov      [si].colSize,dx         ;
         mov      ax,eventAbsolute        ;
         call     es:mseDd.protEntry      ;
         pop      ds                      ;
         jmp      intrHA1                 ;

InterruptHandlerHA endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerMA()
;
; Description:
;
;    Handles interrupts for the MA communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common absolute event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    The MA communication protocol returns a 5 byte packet, and has the
; format...
;
;    Byte 0: 1... .... Sync byte, marks first byte of packet
;            .P.. .... Out of proximity (0 = in-prox)
;            ..T. .... Tablet identifier, command controlled
;            ...X .... Sign bit, 0 = negative, 1 = positive
;            .... Y... Sign bit, 0 = negative, 1 = positive
;            .... .R.. Left button   (button 3) status, 1 = depressed
;            .... ..L. Middle button (button 1) status, 1 = depressed
;            .... ...M Right button  (button 2) status, 1 = depressed
;
;    Byte 1: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 0..6)
;
;    Byte 2: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 7..13)
;
;    Byte 3: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 0..6)
;
;    Byte 4: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 7..13)
;

cseg     segment

         align    4

InterruptHandlerMA proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrMA1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrMA2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         test     al,sync8Bit             ;
         jz       intrMA3                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrMA1           ;  discarded.
intrMA2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrMA3: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrMA1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktMA             ;
         jc       intrMA1                 ;
         mov      byteCnt,0               ;
         test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrMA1                 ;  not active then do nothing
         mov      bl,mEvent[0]            ;
         and      bx,0007h                ; Get button key press translation
         movzx    si,intrMap[bx]          ;
         xor      dx,dx                   ;
         test     mEvent[0],10h           ; Put X position in dx, if less
         jz       intrMA4                 ;  than zero or greater than
         mov      dh,mEvent[2]            ;  maximum position then it must
         shr      dx,1                    ;  be in the margin so pin value
         or       dl,mEvent[1]            ;  at the border position
         cmp      dx,colMax               ;
         jc       intrMA4                 ;
         mov      dx,colMax               ;
         dec      dx                      ;
         jmp      short intrMA5           ;
intrMA4: cmp      dx,colMin               ;
         jnc      intrMA5                 ;
         mov      dx,colMin               ;
intrMA5: sub      dx,colMin               ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         xor      bx,bx                   ;
         test     mEvent[0],08h           ; Put Y position in bx, if less
         jz       intrMA6                 ;  than zero or greater than
         mov      bh,mEvent[4]            ;  maximum position then it must
         shr      bx,1                    ;  be in the margin so pin value
         or       bl,mEvent[3]            ;  at the border position
         cmp      bx,rowMax               ;
         jc       intrMA6                 ;
         mov      bx,rowMax               ;
         dec      bx                      ;
         jmp      short intrMA7           ;
intrMA6: cmp      bx,rowMin               ;
         jnc      intrMA7                 ; Get event flags in cx
         mov      bx,rowMin               ;  if button pressed and moved
intrMA7: sub      bx,rowMin               ;    0000 0000 00M0 R0L0
         xor      bx,rowRvFlg             ;  if button pressed only
         add      bx,rowRvVal             ;    0000 0000 0M0R 0L00
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         cmp      bx,lastRow              ;  if move only
         jnz      intrMA8                 ;    0000 0000 0000 0001
         cmp      dx,lastCol              ;  if no button pressed & not moved
         jnz      intrMA8                 ;    0000 0000 0000 0000
         add      cx,cx                   ; Where L, R and M are 1 if the
         cmp      cx,lastEvnt             ;  corresponding button was pressed
         jnz      intrMA9                 ;  and 0 otherwise
         jmp      intrMA1                 ;
intrMA8: cmp      cx,1                    ;
         adc      cx,0                    ;
         mov      lastRow,bx              ;
         mov      lastCol,dx              ;
intrMA9: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ;
         push     ds                      ; Set up packet addressing
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         mov      [si].rowPos,bx          ;
         mov      [si].colPos,dx          ; Go handle absolute event,
         mov      bx,es:rowRange          ;  interrupts are enabled
         mov      dx,es:colRange          ;  during processing.  Handler
         mov      cx,es:flipFlag          ;  is NOT reentrant!
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      [si].rowSize,bx         ;
         mov      [si].colSize,dx         ;
         mov      ax,eventAbsolute        ;
         call     es:mseDd.protEntry      ;
         pop      ds                      ;
         jmp      intrMA1                 ;

InterruptHandlerMA endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerTA()
;
; Description:
;
;    Handles interrupts for the TA communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common absolute event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    The TA communication protocol returns a 5 byte packet, and has the
; format...
;
;    Byte 0: 1... .... Sync byte, marks first byte of packet
;            .P.. .... In proximity (1 = in-prox)
;            ..xx xx.. Undefined
;            .... ..R. Middle button (button 1) status, 1 = depressed
;            .... ...L Right button  (button 2) status, 1 = depressed
;
;    Byte 1: 0... .... 0 = Non-Sync byte
;            .XXX .... X position (bits 0..2
;            .... xxxx Undefined
;
;    Byte 2: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 3..9)
;
;    Byte 3: 0... .... 0 = Non-Sync byte
;            .YYY .... Y position (bits 0..2)
;            .... xxxx Undefined
;
;    Byte 4: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 3..9)
;

cseg     segment

         align    4

InterruptHandlerTA proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrTA1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrTA2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         test     al,sync8Bit             ;
         jz       intrTA3                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrTA1           ;  discarded.
intrTA2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrTA3: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrTA1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktTA             ;
         jc       intrTA1                 ;
         mov      byteCnt,0               ;
         test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrTA1                 ;  not active then do nothing
         mov      bl,mEvent[0]            ;
         and      bx,0003h                ; Get button key press translation
         movzx    si,intrMap[bx]          ;
         test     mEvent[0],40h           ;
         jnz      intrTA4                 ; If out of proximity (tablet not
         mov      bx,lastRow              ;  touched) then keep last X and Y
         mov      dx,lastCol              ;  position but use current button
         mov      cx,si                   ;  flags
         jmp      intrTA9                 ;
intrTA4: mov      dl,mEvent[1]            ; Put X position in dx, if greater
         add      dl,dl                   ;  than maximum poisition then it
         mov      dh,mEvent[2]            ;  must be in the margin to pin
         shr      dx,5                    ;  value at the border position
         cmp      dx,colMax               ;
         jc       intrTA5                 ;
         mov      dx,colMax               ;
         dec      dx                      ;
         jmp      short intrTA6           ;
intrTA5: cmp      dx,colMin               ;
         jnc      intrTA6                 ;
         mov      dx,colMin               ;
intrTA6: sub      dx,colMin               ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         mov      bl,mEvent[3]            ; Put Y position in bx, if greater
         add      bl,bl                   ;  than maximum poisition then it
         mov      bh,mEvent[4]            ;  must be in the margin to pin
         shr      bx,5                    ;  value at the border position
         cmp      bx,rowMax               ;
         jc       intrTA7                 ;
         mov      bx,rowMax               ;
         dec      bx                      ;
         jmp      short intrTA8           ;
intrTA7: cmp      bx,rowMin               ;
         jnc      intrTA8                 ;
         mov      bx,rowMin               ;
intrTA8: sub      bx,rowMin               ;
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ; Get event flags in cx
         mov      cx,flipFlag             ;  if button pressed and moved
         mov      ax,dx                   ;    0000 0000 00M0 R0L0
         shld     dx,bx,cl                ;  if button pressed only
         shld     bx,ax,cl                ;    0000 0000 0M0R 0L00
         mov      cx,si                   ;  if move only
         cmp      bx,lastRow              ;    0000 0000 0000 0001
         jnz      intrTA10                ;  if no button pressed & not moved
         cmp      dx,lastCol              ;    0000 0000 0000 0000
         jnz      intrTA10                ;
intrTA9: add      cx,cx                   ; Where L, R and M are 1 if the
         cmp      cx,lastEvnt             ;  corresponding button was pressed
         jnz      intrTA11                ;  and 0 otherwise
         jmp      intrTA1                 ;
intrTA10:cmp      cx,1                    ;
         adc      cx,0                    ;
         add      bx,lastRow              ;
         add      dx,lastCol              ; Time-average motion to reduce
         shr      bx,1                    ;  effect of pixel jitter
         shr      dx,1                    ;
         mov      lastRow,bx              ;
         mov      lastCol,dx              ;
intrTA11:mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ;
         push     ds                      ; Set up packet addressing
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         mov      [si].rowPos,bx          ;
         mov      [si].colPos,dx          ; Go handle absolute event,
         mov      bx,es:rowRange          ;  interrupts are enabled
         mov      dx,es:colRange          ;  during processing.  Handler
         mov      cx,es:flipFlag          ;  is NOT reentrant!
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      [si].rowSize,bx         ;
         mov      [si].colSize,dx         ;
         mov      ax,eventAbsolute        ;
         call     es:mseDd.protEntry      ;
         pop      ds                      ;
         jmp      intrTA1                 ;

InterruptHandlerTA endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerUA()
;
; Description:
;
;    Handles interrupts for the UA communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common absolute event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    The UA communication protocol returns an 8 byte packet, and has the
; format...
;
;    Byte 0: x... .... Undefined
;            .1.. .... Sync byte, marks first byte of packet
;            ..00 00.. Reserved
;            .... ..T. Tablet identifier, command controlled
;            .... ...P Out of proximity (0 = in-prox)
;
;    Byte 1: x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..0. .... Reserved
;            ...B BBBB Button flags
;
;    Byte 2  x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..XX XXXX X position (bits 0..5)
;
;    Byte 3: x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..XX XXXX X position (bits 6..11)
;
;    Byte 4: x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..0. .... Reserved
;            ...S .... Sign bit for X
;            .... XXXX X position (bits 12..15)
;
;    Byte 5: x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..YY YYYY Y position (bits 0..5)
;
;    Byte 6: x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..YY YYYY Y position (bits 6..11)
;
;    Byte 7: x... .... Undefined
;            .0.. .... Non-Sync byte
;            ..0. .... Reserved
;            ...S .... Sign bit for Y
;            .... YYYY Y position (bits 12..15)
;

cseg     segment

         align    4

InterruptHandlerUA proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrUA1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrUA2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         and      al,7fh                  ;
         test     al,sync7Bit             ;
         jz       intrUA3                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrUA1           ;  discarded.
intrUA2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrUA3: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrUA1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktUA             ;
         jc       intrUA1                 ;
         mov      byteCnt,0               ;
         test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrUA1                 ;  not active then do nothing
         mov      bl,mEvent[1]            ;
         and      bx,000fh                ; Get button key press translation
         movzx    si,intrMap[bx]          ;
         xor      dx,dx                   ;
         test     mEvent[4],10h           ; Put X position in dx, if less
         jnz      intrUA4                 ;  than zero or greater than
         mov      dx,word ptr mEvent[3]   ;  maximum position then it must
         shl      dl,2                    ;  be in the margin so pin value
         shl      dx,4                    ;  at the border position
         or       dl,mEvent[2]            ;
         cmp      dx,colMax               ;
         jc       intrUA4                 ;
         mov      dx,colMax               ;
         dec      dx                      ;
         jmp      short intrUA5           ;
intrUA4: cmp      dx,colMin               ;
         jnc      intrUA5                 ;
         mov      dx,colMin               ;
intrUA5: sub      dx,colMin               ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         xor      bx,bx                   ;
         test     mEvent[7],10h           ; Put Y position in bx, if less
         jnz      intrUA6                 ;  than zero or greater than
         mov      bx,word ptr mEvent[6]   ;  maximum position then it must
         shl      bl,2                    ;  be in the margin so pin value
         shl      bx,4                    ;  at the border position
         or       bl,mEvent[5]            ;
         cmp      bx,rowMax               ;
         jc       intrUA6                 ;
         mov      bx,rowMax               ;
         dec      bx                      ;
         jmp      short intrUA7           ;
intrUA6: cmp      bx,rowMin               ;
         jnc      intrUA7                 ; Get event flags in cx
         mov      bx,rowMin               ;  if button pressed and moved
intrUA7: sub      bx,rowMin               ;    0000 0000 00M0 R0L0
         xor      bx,rowRvFlg             ;  if button pressed only
         add      bx,rowRvVal             ;    0000 0000 0M0R 0L00
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         cmp      bx,lastRow              ;  if move only
         jnz      intrUA8                 ;    0000 0000 0000 0001
         cmp      dx,lastCol              ;  if no button pressed & not moved
         jnz      intrUA8                 ;    0000 0000 0000 0000
         add      cx,cx                   ; Where L, R and M are 1 if the
         cmp      cx,lastEvnt             ;  corresponding button was pressed
         jnz      intrUA9                 ;  and 0 otherwise
         jmp      intrUA1                 ;
intrUA8: cmp      cx,1                    ;
         adc      cx,0                    ;
         mov      lastRow,bx              ;
         mov      lastCol,dx              ;
intrUA9: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ;
         push     ds                      ; Set up packet addressing
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         mov      [si].rowPos,bx          ;
         mov      [si].colPos,dx          ; Go handle absolute event,
         mov      bx,es:rowRange          ;  interrupts are enabled
         mov      dx,es:colRange          ;  during processing.  Handler
         mov      cx,es:flipFlag          ;  is NOT reentrant!
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      [si].rowSize,bx         ;
         mov      [si].colSize,dx         ;
         mov      ax,eventAbsolute        ;
         call     es:mseDd.protEntry      ;
         pop      ds                      ;
         jmp      intrUA1                 ;

InterruptHandlerUA endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerBA()
;
; Description:
;
;    Handles interrupts for the BA communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common absolute event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    The BA communication protocol returns a 5 byte packet, and has the
; format...
;
;    Byte 0: x... .... Undefined
;            .1.. .... Sync byte, marks first byte of packet
;            ..1. .... Button 4 status, 1 = depressed
;            ...1 .... Button 3 status, 1 = depressed
;            .... 1... Button 2 status, 1 = depressed
;            .... .1.. Button 1 status, 1 = depressed
;            .... ..0. Always 0
;            .... ...0 Bit Pad One - always 0
;                      Bit Pad Two - proximity, 0 = in-proximity
;
;    Byte 1: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..XX XXXX X position (bits 0..5)
;
;    Byte 2: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..XX XXXX X position (bits 6..11)
;
;    Byte 3: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..YY YYYY Y position (bits 0..5)
;
;    Byte 4: x... .... Undefined
;            .0.. .... 0 = Non-Sync byte
;            ..YY YYYY Y position (bits 6..11)
;

cseg     segment

         align    4

InterruptHandlerBA proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrBA1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrBA2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         test     al,sync7Bit             ;
         jz       intrBA3                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrBA1           ;  discarded.
intrBA2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrBA3: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrBA1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktBA             ;
         jc       intrBA1                 ;
         mov      byteCnt,0               ;
         test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrBA1                 ;  not active then do nothing
         mov      bl,mEvent[0]            ;
         shr      bx,2                    ;
         and      bx,000fh                ; Get button key press translation
         movzx    si,intrMap[bx]          ;
         xor      dx,dx                   ;
         mov      dh,mEvent[2]            ; Put X position in dx, if less
         shr      dx,2                    ;  than minimum or greater than
         or       dl,mEvent[1]            ;  maximum position then it must
         cmp      dx,colMax               ;  be in the margin so pin value
         jc       intrBA4                 ;  at the border position
         mov      dx,colMax               ;
         dec      dx                      ;
         jmp      short intrBA5           ;
intrBA4: cmp      dx,colMin               ;
         jnc      intrBA5                 ;
         mov      dx,colMin               ;
intrBA5: sub      dx,colMin               ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         xor      bx,bx                   ;
         mov      bh,mEvent[4]            ; Put Y position in bx, if less
         shr      bx,2                    ;  than minimum or greater than
         or       bl,mEvent[3]            ;  maximum position then it must
         cmp      bx,rowMax               ;  be in the margin so pin value
         jc       intrBA6                 ;  at the border position
         mov      bx,rowMax               ;
         dec      bx                      ;
         jmp      short intrBA7           ; Get event flags in cx
intrBA6: cmp      bx,rowMin               ;  if button pressed and moved
         jnc      intrBA7                 ;    0000 0000 00M0 R0L0
         mov      bx,rowMin               ;  if button pressed only
intrBA7: sub      bx,rowMin               ;    0000 0000 0M0R 0L00
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         cmp      bx,lastRow              ;  if move only
         jnz      intrBA8                 ;    0000 0000 0000 0001
         cmp      dx,lastCol              ;  if no button pressed & not moved
         jnz      intrBA8                 ;    0000 0000 0000 0000
         add      cx,cx                   ; Where L, R and M are 1 if the
         cmp      cx,lastEvnt             ;  corresponding button was pressed
         jnz      intrBA9                 ;  and 0 otherwise
         jmp      intrBA1                 ;
intrBA8: cmp      cx,1                    ;
         adc      cx,0                    ;
         mov      lastRow,bx              ;
         mov      lastCol,dx              ;
intrBA9: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ;
         push     ds                      ; Set up packet addressing
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         mov      [si].rowPos,bx          ;
         mov      [si].colPos,dx          ;
         mov      bx,es:rowRange          ;
         mov      dx,es:colRange          ; Go handle absolute event,
         mov      cx,es:flipFlag          ;  interrupts are enabled
         mov      ax,dx                   ;  during processing.  Handler
         shld     dx,bx,cl                ;  is NOT reentrant!
         shld     bx,ax,cl                ;
         mov      [si].rowSize,bx         ;
         mov      [si].colSize,dx         ;
         mov      ax,eventAbsolute        ;
         call     es:mseDd.protEntry      ;
         pop      ds                      ;
         jmp      intrBA1                 ;

InterruptHandlerBA endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerCA()
;
; Description:
;
;    Handles interrupts for the CA communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common absolute event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    The CA communication protocol returns a 5 byte packet, and has the
; format...
;
;    Byte 0: 1... .... Sync byte, marks first byte of packet
;            .0.. .... Proximity, 0 = in-proximity
;            ..0. .... Always 0
;            ...1 .... Always 1
;            .... 1... Always 1
;            .... .FFF Button flags
;
;    Byte 1: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 0..6)
;
;    Byte 2: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 7..13)
;
;    Byte 3: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 0..6)
;
;    Byte 4: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 7..13)
;

cseg     segment

         align    4

InterruptHandlerCA proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrCA1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrCA2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         test     al,sync8Bit             ;
         jz       intrCA3                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrCA1           ;  discarded.
intrCA2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrCA3: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrCA1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktCA             ;
         jc       intrCA1                 ;
         mov      byteCnt,0               ;
         test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrCA1                 ;  not active then do nothing
         mov      bl,mEvent[0]            ;
         and      bx,0007h                ; Get button key press translation
         movzx    si,intrMap[bx]          ;
         xor      dx,dx                   ;
         mov      dh,mEvent[2]            ; Put X position in dx, if less
         shr      dx,1                    ;  than minimum or greater than
         or       dl,mEvent[1]            ;  maximum position then it must
         cmp      dx,colMax               ;  be in the margin so pin value
         jc       intrCA4                 ;  at the border position
         mov      dx,colMax               ;
         dec      dx                      ;
         jmp      short intrCA5           ;
intrCA4: cmp      dx,colMin               ;
         jnc      intrCA5                 ;
         mov      dx,colMin               ;
intrCA5: sub      dx,colMin               ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         xor      bx,bx                   ;
         mov      bh,mEvent[4]            ; Put Y position in bx, if less
         shr      bx,1                    ;  than minimum or greater than
         or       bl,mEvent[3]            ;  maximum position then it must
         cmp      bx,rowMax               ;  be in the margin so pin value
         jc       intrCA6                 ;  at the border position
         mov      bx,rowMax               ;
         dec      bx                      ;
         jmp      short intrCA7           ; Get event flags in cx
intrCA6: cmp      bx,rowMin               ;  if button pressed and moved
         jnc      intrCA7                 ;    0000 0000 00M0 R0L0
         mov      bx,rowMin               ;  if button pressed only
intrCA7: sub      bx,rowMin               ;    0000 0000 0M0R 0L00
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         cmp      bx,lastRow              ;  if move only
         jnz      intrCA8                 ;    0000 0000 0000 0001
         cmp      dx,lastCol              ;  if no button pressed & not moved
         jnz      intrCA8                 ;    0000 0000 0000 0000
         add      cx,cx                   ; Where L, R and M are 1 if the
         cmp      cx,lastEvnt             ;  corresponding button was pressed
         jnz      intrCA9                 ;  and 0 otherwise
         jmp      intrCA1                 ;
intrCA8: cmp      cx,1                    ;
         adc      cx,0                    ;
         mov      lastRow,bx              ;
         mov      lastCol,dx              ;
intrCA9: mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ;
         push     ds                      ; Set up packet addressing
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         mov      [si].rowPos,bx          ;
         mov      [si].colPos,dx          ;
         mov      bx,es:rowRange          ;
         mov      dx,es:colRange          ; Go handle absolute event,
         mov      cx,es:flipFlag          ;  interrupts are enabled
         mov      ax,dx                   ;  during processing.  Handler
         shld     dx,bx,cl                ;  is NOT reentrant!
         shld     bx,ax,cl                ;
         mov      [si].rowSize,bx         ;
         mov      [si].colSize,dx         ;
         mov      ax,eventAbsolute        ;
         call     es:mseDd.protEntry      ;
         pop      ds                      ;
         jmp      intrCA1                 ;

InterruptHandlerCA endp

cseg     ends



;
; Interface:
;
;    InterruptHandlerFA()
;
; Description:
;
;    Handles interrupts for the FA communication protocol.  Packet bytes are
; accumulated until the packet is complete.  The packet is translated to
; common absolute event format and then sent to the device independent event
; handler.  The interrupt handler is not exited until mo more characters are
; available from the mouse.  In conjunction with FIFO buffering this can
; reduce the system overhead required for the mouse -- especially for higher
; reporting rates.
;
; Notes:
;
;    The FA communication protocol returns a 5 byte packet, and has the
; format...
;
;    Byte 0: 1... .... Sync byte, marks first byte of packet
;            .1.. .... Always 1
;            ..R. .... Right button   (button 3) status, 1 = depressed
;            ...L .... Left button  (button 1) status, 1 = depressed
;            .... M... Middle button (button 2) status, 1 = depressed
;            .... .000 Always 0
;
;    Byte 1: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 0..6)
;
;    Byte 2: 0... .... 0 = Non-Sync byte
;            .XXX XXXX X position (bits 7..13)
;
;    Byte 3: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 0..6)
;
;    Byte 4: 0... .... 0 = Non-Sync byte
;            .YYY YYYY Y position (bits 7..13)
;

cseg     segment

         align    4

InterruptHandlerFA proc far

         cli                              ;
         mov      dx,mskPrt8259           ; Disable interrupts at 8259 to
         in       al,dx                   ;  avoid reentrant interrupts
         or       al,disable8259          ;  after eoi has been issued
         out      dx,al                   ;
         sti                              ;
         mov      al,deviceData.irq       ;
         mov      dl,DevHlp_Eoi           ; Call DeviceHelp to issue eoi
         call     deviceHelp              ;
intrFA1: mov      dx,deviceData.portAddr  ;
         add      dx,lsr                  ;
         in       al,dx                   ; Read line status register both to
         MyIoDelay                        ;  clear errors and check for data,
         test     al,lsrData              ;  if no more data then exit
         jz       intrFA2                 ;
         add      dx,rxb-lsr              ; Read data byte from serial port
         in       al,dx                   ;
         test     al,sync8Bit             ;
         jz       intrFA3                 ; Accumulate packet bytes in
         mov      byteCnt,1               ;  mEvent.  Incomplete packets
         mov      mEvent[0],al            ;  and spurious data will be
         jmp      short intrFA1           ;  discarded.
intrFA2: cli                              ;
         mov      dx,mskPrt8259           ;
         in       al,dx                   ; Enable mouse interrupts at 8259
         and      al,enable8259           ;
         out      dx,al                   ;
         clc                              ; Indicate our interrupt
         ret                              ;
intrFA3: mov      di,byteCnt              ;
         or       di,di                   ;
         jz       intrFA1                 ;
         mov      mEvent[di],al           ; Process packet when all bytes
         inc      di                      ;  have been received
         mov      byteCnt,di              ;
         cmp      di,sizPktFA             ;
         jc       intrFA1                 ;
         mov      byteCnt,0               ;
         test     devStatus,readEnable    ; If reporting disabled or cursor
         jz       intrFA1                 ;  not active then do nothing
         mov      bl,mEvent[0]            ;
         shr      bx,3                    ;
         and      bx,0007h                ; Get button key press translation
         movzx    si,intrMap[bx]          ;
         xor      dx,dx                   ;
         test     mEvent[2],40h           ; Put X position in dx, if less
         jnz      intrFA5                 ;  than zero or greater than
         mov      dh,mEvent[2]            ;  maximum position then it must
         shr      dx,1                    ;  be in the margin so pin value
         or       dl,mEvent[1]            ;  at the border position
         cmp      dx,colMax               ;
         jc       intrFA5                 ;
         sub      dx,colMax               ;
         mov      cl,response             ;
         shl      dx,cl                   ;
         add      dx,colBase              ; Tentative code to implement
         cmp      dx,colBmax              ;  acceleration for absolute
         jc       intrFA4                 ;  device
         mov      dx,colBmax              ;
intrFA4: mov      colBase,dx              ;
         mov      dx,colMax               ;
         dec      dx                      ;
         jmp      short intrFA7           ;
intrFA5: cmp      dx,colMin               ;
         jnc      intrFA7                 ;
         sub      dx,colMin               ;
         mov      cl,response             ; Tentative code to implement
         shl      dx,cl                   ;  acceleration for absolute
         add      colBase,dx              ;  device
         jc       intrFA6                 ;
         mov      colBase,0               ;
intrFA6: mov      dx,colMin               ;
intrFA7: sub      dx,colMin               ;
         add      dx,colBase              ;
         xor      dx,colRvFlg             ;
         add      dx,colRvVal             ;
         xor      bx,bx                   ;
         test     mEvent[4],40h           ; Put Y position in bx, if less
         jnz      intrFA9                 ;  than zero or greater than
         mov      bh,mEvent[4]            ;  maximum position then it must
         shr      bx,1                    ;  be in the margin so pin value
         or       bl,mEvent[3]            ;  at the border position
         cmp      bx,rowMax               ;
         jc       intrFA9                 ;
         sub      bx,rowMax               ;
         mov      cl,response             ;
         shl      bx,cl                   ;
         add      bx,rowBase              ; Tentative code to implement
         cmp      bx,rowBmax              ;  acceleration for absolute
         jc       intrFA8                 ;  device
         mov      bx,rowBmax              ;
intrFA8: mov      rowBase,bx              ;
         mov      bx,rowMax               ;
         dec      bx                      ;
         jmp      short intrFA11          ; Get event flags in cx
intrFA9: cmp      bx,rowMin               ;  if button pressed and moved
         jnc      intrFA11                ;    0000 0000 00M0 R0L0
         sub      bx,rowMin               ;
         mov      cl,response             ; Tentative code to implement
         shl      bx,cl                   ;  acceleration for absolute
         add      rowBase,bx              ;  device
         jc       intrFA10                ;
         mov      rowBase,0               ;
intrFA10:mov      bx,rowMin               ;  if button pressed only
intrFA11:sub      bx,rowMin               ;    0000 0000 0M0R 0L00
         add      bx,rowBase              ;
         xor      bx,rowRvFlg             ;
         add      bx,rowRvVal             ;
         mov      cx,flipFlag             ;
         mov      ax,dx                   ;
         shld     dx,bx,cl                ;
         shld     bx,ax,cl                ;
         mov      cx,si                   ;
         cmp      bx,lastRow              ;  if move only
         jnz      intrFA12                ;    0000 0000 0000 0001
         cmp      dx,lastCol              ;  if no button pressed & not moved
         jnz      intrFA12                ;    0000 0000 0000 0000
         add      cx,cx                   ; Where L, R and M are 1 if the
         cmp      cx,lastEvnt             ;  corresponding button was pressed
         jnz      intrFA13                ;  and 0 otherwise
         jmp      intrFA1                 ;
intrFA12:cmp      cx,1                    ;
         adc      cx,0                    ;
         mov      lastRow,bx              ;
         mov      lastCol,dx              ;
intrFA13:mov      lastEvnt,cx             ;
         mov      si,intPacketOff         ;
         push     ds                      ; Set up packet addressing
         mov      ax,ds                   ;
         mov      ds,mseDd.protDs         ; Put event flags, row movement and
         mov      es,ax                   ;  column movement into event packet
         mov      [si].event,cx           ;
         mov      [si].rowPos,bx          ;
         mov      [si].colPos,dx          ;
         mov      bx,es:rowRange          ;
         mov      dx,es:colRange          ; Go handle absolute event,
         mov      cx,es:flipFlag          ;  interrupts are enabled
         mov      ax,dx                   ;  during processing.  Handler
         shld     dx,bx,cl                ;  is NOT reentrant!
         shld     bx,ax,cl                ;
         mov      [si].rowSize,bx         ;
         mov      [si].colSize,dx         ;
         mov      ax,eventAbsolute        ;
         call     es:mseDd.protEntry      ;
         pop      ds                      ;
         jmp      intrFA1                 ;

InterruptHandlerFA endp

cseg     ends



;
; Interface:
;
;    [accelrat](row bx, column dx, dataSegment es)
;
;    Where accelrat contains a near pointer to NoAccelerate, DeAccelerate or
;    Accelerate.
;
; Description:
;
;    Handles acceleration changes.
;
; Exit:
;
;    Registers bx and dx are altered.
;
; Side effects:
;
;    Registers ax and cx are altered.
;

cseg     segment

         align    4

NoAccelerate proc near

         ret                              ; No acceleration

NoAccelerate endp

         align    4

Accelerate proc near

         cmp      bx,-7                   ;
         jl       accel1                  ; If in range -7..+7 then do table
         cmp      bx,7                    ;  lookup to allow for "stepping"
         jg       accel2                  ;  the acceleration, otherwise
         movsx    ax,es:accelTbl+7[bx]    ;  go handle larger values.  Code
accel0:  cmp      dx,-7                   ;  is optimized for "fall through"
         jl       accel3                  ;  assuming small values.
         cmp      dx,7                    ;
         jg       accel4                  ;
         mov      bx,dx                   ;
         movsx    dx,es:accelTbl+7[bx]    ;
         mov      bx,ax                   ;
         ret                              ;
accel1:  mov      cl,es:reshift           ;
         shl      bx,cl                   ; Handle large negative value in
         add      bx,es:accelFix          ;  bx
         mov      ax,bx                   ;
         jmp      short accel0            ;
accel2:  mov      cl,es:reshift           ;
         shl      bx,cl                   ; Handle large positive value in
         sub      bx,es:accelFix          ;  bx
         mov      ax,bx                   ;
         jmp      short accel0            ;
accel3:  mov      cl,es:reshift           ;
         shl      dx,cl                   ; Handle large negative value in
         add      dx,es:accelFix          ;  dx, return when done
         mov      bx,ax                   ;
         ret                              ;
accel4:  mov      cl,es:reshift           ;
         shl      dx,cl                   ; Handle large positive value in
         sub      dx,es:accelFix          ;  dx, return when done
         mov      bx,ax                   ;
         ret                              ;

Accelerate endp

         align    4

DeAccelerate proc near

         mov      cl,es:reshift           ;
         bt       dx,15                   ; Reduce mouse sensitivity by
         sbb      ax,ax                   ;  requested factor, rounding
         and      ax,es:rounding          ;  towards zero
         add      dx,ax                   ;
         sar      dx,cl                   ;
         bt       bx,15                   ;
         sbb      ax,ax                   ;
         and      ax,es:rounding          ;
         add      bx,ax                   ;
         sar      bx,cl                   ;
         ret                              ;

DeAccelerate endp

cseg     ends



;----------------------------------------------------------------------------#
;                                                                            #
;    All data past this point will be discarded following device loading.    #
;                                                                            #
;----------------------------------------------------------------------------#

dseg     segment

         align    4

endData  equ      $              ; End of data retained after init

dseg     ends



;----------------------------------------------------------------------------#
;                                                                            #
;    All code past this point will be discarded following device loading.    #
;                                                                            #
;----------------------------------------------------------------------------#

cseg     segment

         align    4

endCode  equ      $              ; End of code retained after init

cseg     ends



;----------------------------------------------------------------------------#
;                                                                            #
;    Default serial port initialization state for each serial protocol.      #
;                                                                            #
;----------------------------------------------------------------------------#

dseg     segment

         align    4

portRE   PortState   <1200, div1200, lcrBits7+lcrStop1+lcrPeven, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>

portMI   PortState   <1200, div1200, lcrBits7+lcrStop1+lcrPnone, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>
portMP   PortState   <1200, div1200, lcrBits7+lcrStop1+lcrPnone, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>

port3B   PortState   <1200, div1200, lcrBits8+lcrStop1+lcrPnone, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>
port5B   PortState   <1200, div1200, lcrBits8+lcrStop1+lcrPnone, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>

portMM   PortState   <1200, div1200, lcrBits8+lcrStop1+lcrPodd,  mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>

portBA   PortState   <9600, div9600, lcrBits7+lcrStop2+lcrPeven, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>
portBR   PortState   <9600, div9600, lcrBits7+lcrStop2+lcrPeven, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>
portUA   PortState   <9600, div9600, lcrBits7+lcrStop2+lcrPeven, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>
portUR   PortState   <9600, div9600, lcrBits7+lcrStop2+lcrPeven, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>

portUN   PortState   <9600, div9600, lcrBits7+lcrStop2+lcrPnone, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>

portFA   PortState   <9600, div9600, lcrBits8+lcrStop1+lcrPnone, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>
portFR   PortState   <9600, div9600, lcrBits8+lcrStop1+lcrPnone, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>
portHA   PortState   <9600, div9600, lcrBits8+lcrStop1+lcrPnone, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>
portHR   PortState   <9600, div9600, lcrBits8+lcrStop1+lcrPnone, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>
portTA   PortState   <9600, div9600, lcrBits8+lcrStop1+lcrPnone, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>
portTR   PortState   <9600, div9600, lcrBits8+lcrStop1+lcrPnone, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>

portCA   PortState   <9600, div9600, lcrBits8+lcrStop1+lcrPodd,  mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>
portCR   PortState   <9600, div9600, lcrBits8+lcrStop1+lcrPodd,  mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>
portMA   PortState   <9600, div9600, lcrBits8+lcrStop1+lcrPodd,  mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>
portMR   PortState   <9600, div9600, lcrBits8+lcrStop1+lcrPodd,  mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>

portDG   PortState   <9600, div9600, lcrBits8+lcrStop2+lcrPnone, mcrDtr+mcrRts, ierRx, fcrOff, 0, 0>

dseg     ends



;----------------------------------------------------------------------------#
;                                                                            #
;    This section contains all utility routines.                             #
;                                                                            #
;----------------------------------------------------------------------------#

;
; Interface:
;
;    Report()
;
; Description:
;
;    Reports load state to console.
;
; Side effects:
;
;    Registers ax is altered.
;

dseg     segment

loadMsg  db       47,'Mouse driver RODENT.SYS version 1.0.13 loaded',cr,lf
copyMsg  db       71,'Copyright (c) 1993..1996 by Michael Lee Finney.  All rights reserved.',cr,lf
battMsg  db       65,'WARNING! MOUSE BATTERY MUST BE REPLACED FOR RELIABLE OPERATION!',cr,lf
stopMsg  db       23,'Press Enter to continue'
crlfMsg  db       2,cr,lf

autoMsg  db       10,'********',cr,lf

dseg     ends

cseg     segment

         align    4

Report proc near

         mov      bx,offset crlfMsg       ;
         call     PutString               ; Write blank line
         mov      bx,offset loadMsg       ;
         call     PutString               ; Write driver loaded message
         mov      bx,offset copyMsg       ;
         call     PutString               ; Write copyright message
         cmp      battLow,0               ;
         jz       report2                 ;
         mov      bx,offset crlfMsg       ; Write blank line
         call     PutString               ;
         mov      bx,offset battMsg       ; Write battery low message
         call     PutString               ;
         mov      bx,offset crlfMsg       ; Write blank line
         call     PutString               ;
         call     OpenKeyboard            ;
         mov      bx,1500                 ;
         mov      cx,bx                   ; Beep to alert user of problem
         mov      dl,DevHlp_Beep          ;
         call     deviceHelp              ;
         mov      bx,offset stopMsg       ; Write continue message
         call     PutString               ;
report1: call     ReadKeyboard            ;
         cmp      al,cr                   ; Wait until <cr> is seen
         jnz      report1                 ;
         call     CloseKeyboard           ;
         mov      bx,offset crlfMsg       ;
         call     PutString               ; Write blank line
report2: ret                              ;

Report endp

cseg     ends



;
; Interface:
;
;    OpenKeyboard()
;
; Description:
;
;    Opens the keyboard.
;
; Normal exit:
;
;    Register ax is zero.
;
; Error exit:
;
;    Register ax contains the non-zero return code.
;
; Side effects:
;
;    Register ax is altered.
;

dseg     segment

keyboard dw       0
action   dw       0
kbd      db       'KBD$',0

dseg     ends

cseg     segment

         align    4

OpenKeyboard proc near

         push     ds                      ;
         mov      ax,offset kbd           ; DosOpen(
         push     ax                      ;    "KBD$",
         push     ds                      ;
         mov      ax,offset keyboard      ;    &keyboard,
         push     ax                      ;
         push     ds                      ;
         mov      ax,offset action        ;    &action,
         push     ax                      ;
         xor      ax,ax                   ;
         push     ax                      ;    0L,
         push     ax                      ;
         push     ax                      ;    0,
         mov      ax,0001h                ;
         push     ax                      ;    FILE_OPEN,
         mov      ax,2042h                ;
         push     ax                      ;    OPEN_FLAGS_FAIL_ON_ERROR | OPEN_SHARE_DENYNONE | OPEN_ACCESS_READWRITE,
         xor      ax,ax                   ;
         push     ax                      ;    0L)
         push     ax                      ;
         call     DosOpen                 ;
         ret                              ;

OpenKeyboard endp

cseg     ends



;
; Interface:
;
;    ReadKeyboard()
;
; Description:
;
;    Reads a character from the keyboard.  All characters are ignored which
; constitute an interrim character or which also have a shift key pressed.
;
; Exit:
;
;    Register ax contains the character.
;
; Side effects:
;
;    Register ax is altered.
;

dseg     segment

kbdParms equ      $
kbdLen   dw       0

kbdData  equ      $
kbdChar  db       0
kbdCode  db       0
kbdStat  db       0
kbdNlshf db       0
kbdShft  dw       0
kbdTime  dd       0

dseg     ends

cseg     segment

         align    4

ReadKeyboard proc near

readKey: mov      kbdLen,1                ;
         mov      kbdStat,0               ;
         push     ds                      ;
         mov      ax,offset kbdData       ; DosDevIOCtl(
         push     ax                      ;    &kbdData,
         push     ds                      ;
         mov      ax,offset kbdParms      ;    &kbdParms,
         push     ax                      ;
         mov      ax,0074h                ;    74h,
         push     ax                      ;
         mov      ax,0004h                ;    04h,
         push     ax                      ;
         push     keyboard                ;    keyboard)
         call     DosDevIOCtl             ;
         or       ax,ax                   ;
         jnz      readKey                 ;
         cmp      kbdChar,cr              ; Wait for a character, ignore
         jnz      readKey                 ;  interrim characters and all
         mov      al,kbdStat              ;  characters with a shift key
         and      al,43h                  ;  pressed
         cmp      al,40h                  ;
         jnz      readKey                 ;
         mov      ax,kbdShft              ;
         and      ax,0ff0fh               ;
         jnz      readKey                 ;
         mov      al,kbdChar              ; Return the character
         ret                              ;

ReadKeyboard endp

cseg     ends



;
; Interface:
;
;    CloseKeyboard()
;
; Description:
;
;    Closes the keyboard.
;
; Normal exit:
;
;    Register ax is zero.
;
; Error exit:
;
;    Register ax contains the non-zero return code.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

CloseKeyboard proc near

         push     keyboard                ;
         call     DosClose                ; DosClose(keyboard)
         ret                              ;

CloseKeyboard endp

cseg     ends



;
; Interface:
;
;    GetString(string *ds:bx, length cx)
;
; Description:
;
;    Reads string from console and returns number of bytes read in ax.
;
; Side effects:
;
;    Register ax is altered.
;

dseg     segment

stdin    equ      0

readLen  dw       0

dseg     ends

cseg     segment

         align    4

GetString proc near

         mov      ax,stdin                ; DosRead(
         push     ax                      ;    stdin,
         push     ds                      ;    buffer *,
         push     bx                      ;    length,
         push     cx                      ;    &readLen)
         push     ds                      ;
         mov      ax,offset readLen       ; We don't check return code
         push     ax                      ;  because nothing we can do and
         call     DosRead                 ;  reads from console should never
         mov      ax,readLen              ;  fail
         ret                              ;

GetString endp

cseg     ends



;
; Interface:
;
;    PutString(string *ds:bx)
;
; Description:
;
;    Writes string to console.
;
; Side effects:
;
;    Registers ax and bx are altered.
;

dseg     segment

stdout   equ      1

writeLen dw       0

dseg     ends

cseg     segment

         align    4

PutString proc near

         mov      ax,stdout               ;
         push     ax                      ; DosWrite(
         push     ds                      ;    stdout,
         movzx    ax,byte ptr [bx]        ;    buffer *,
         inc      bx                      ;    length,
         push     bx                      ;    &writeLen)
         push     ax                      ;
         push     ds                      ;
         mov      ax,offset writeLen      ; We don't check return code or
         push     ax                      ;  length actually written because
         call     DosWrite                ;  nothing we can do and writes to
         ret                              ;  console should never fail

PutString endp

cseg     ends



;
; Interface:
;
;    PutQueryString(unused si, requested cx)
;
; Description:
;
;    Writes query string to console.  The field mouseCfg will contain a
; series of hex bytes starting at mouseCfg[cx-1] and ending at mouseCfg[si],
; inclusive, where 0 <= si <= cx.  These bytes will be displayed as an ASCII
; string and terminated with a <cr, lf>.  If the carry flag is set on entry
; then the string will be followed by ... to indicate that mouseCfg
; overflowed.  After the string has been displayed, this routine will delay
; until a <cr> is typed on the keyboard.
;
; Side effects:
;
;    Neither flags nor registers are altered.
;

dseg     segment

hexTable db       '0123456789abcdef'
hexLine  db       40 dup(0)

dseg     ends

cseg     segment

         align    4

PutQueryString proc near

         pusha                            ;
         pushf                            ; Save all flags and registers
         xchg     cx,si                   ;
         mov      di,offset hexLine       ;
         cmp      cx,si                   ;
         jae      putQs2                  ;
putQs1:  mov      bl,mouseCfg[si-1]       ; Write each byte in query string
         shr      bl,4                    ;  to hexLine for display after
         and      bx,0fh                  ;  converting to display hex format
         mov      al,hexTable[bx]         ;
         mov      byte ptr [di+1],al      ;
         mov      bl,mouseCfg[si-1]       ;
         and      bx,0fh                  ;
         mov      al,hexTable[bx]         ;
         mov      byte ptr [di+2],al      ;
         add      di,2                    ;
         dec      si                      ;
         cmp      cx,si                   ;
         jae      putQs2                  ;
         mov      byte ptr [di+1],'.'     ; Separate each byte with a '.'
         inc      di                      ;  for readability
         jmp      short putQs1            ;
putQs2:  popf                             ;
         pushf                            ;
         jnc      putQs3                  ; Add ... if mouseCfg overflowed
         mov      byte ptr [di+1],'.'     ;
         mov      byte ptr [di+2],'.'     ;
         mov      byte ptr [di+3],'.'     ;
         add      di,3                    ;
putQs3:  mov      byte ptr [di+1],cr      ; Add <cr, lf> to end of string and
         mov      byte ptr [di+2],lf      ;  prefix string with length of
         add      di,2                    ;  string
         mov      bx,offset hexLine       ;
         mov      ax,di                   ;
         sub      ax,bx                   ;
         mov      byte ptr [bx],al        ; Write string to console
         call     putString               ;
         call     OpenKeyboard            ;
putQs4:  call     ReadKeyboard            ;
         cmp      al,cr                   ; Wait until <cr> is seen
         jnz      putQs4                  ;
         call     CloseKeyboard           ;
         popf                             ;
         popa                             ; Restore all flags and registers
         ret                              ;  before returning

PutQueryString endp

cseg     ends



;
; Interface:
;
;    SetupDevice()
;
; Description:
;
;    Perform final serial port initialization.  Mouse interrupts are enabled
; at the serial port when this routine completes.
;
;    The button mapping table will be dynamically constructed.  The physical
; button mapping table for the selected protocol will have the button
; ordering altered by mapping each entry through the btnOrder map constructed
; by the ORDER option.  After button reordering, each table entry will be
; mapped using the OS/2 button map for either a two or three button mouse.
;
;    No reordering is required for the default ordering which is suitable for
; virtually all mice.  The pucks available for digitizer tablets frequently
; require button reordering because button numbering and positioning are
; inconsistent across different pucks (even from the same company).  The
; final result will be used for high-speed lookup in the interrupt routines.
;
; Side effects:
;
;    Registers ax, bx, cx, dx, si, di and es are altered.
;

dseg     segment

stup5B   db       'OU',0
stupMM   db       'OS',0
stupRE   db       'OB',0
stupAC   db       'E@',0
stupSG   db       'E@',0
stupMA   db       'F@',0
stupMR   db       'F@',0
stupBA   db       '@O',0
stupBR   db       '@O',0
stupCA   db       '@',0
stupCR   db       '@',0
stupUA   db       escape,'M5',escape,'M0',0
stupUR   db       escape,'M5',escape,'M0',0
stupTA1  db       soh,'FT',cr,0
stupTA2  db       soh,'MS',cr,0
stupTR1  db       soh,'FT',cr,0
stupTR2  db       soh,'MS',cr,0

dseg     ends

cseg     segment

         align    4

SetupDevice proc near

         mov      al,protocol             ; Use the requested protocol
         mov      dx,deviceData.portAddr  ;  to initialize the mouse
         mov      si,offset btnMapX       ;

setup0:  cmp      al,protoMP              ;
         jnz      setup1                  ; Protocol MP selected, set the
         mov      si,offset btnMapMP      ;  correct baud rate and program
         mov      al,mouse                ;  mouse
         cmp      al,typeMW               ;
         jz       setup0a                 ;
         cmp      al,typeVW               ;
         jnz      setup0b                 ;
setup0a: mov      al,'X'                  ;
         call     SendMouseCommand        ;
setup0b: mov      buttons,3               ;
         mov      colRange,1              ;
         mov      rowRange,1              ;
         push     portMP.ratState         ;
         mov      bx,offset portMP        ;
         call     SetSerialState          ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      al,mouse                ;
         cmp      al,typeMW               ;
         jz       setup0c                 ;
         cmp      al,typeVW               ;
         jnz      setup90                 ;
setup0c: mov      al,'X'                  ;
         call     SendMouseCommand        ;
         jmp      setup90                 ;

setup1:  cmp      al,protoMI              ;
         jnz      setup2                  ; Protocol MI selected, set the
         mov      si,offset btnMapMI      ;  correct baud rate and program
         mov      al,mouse                ;  mouse
         cmp      al,typeMW               ;
         jz       setup1a                 ;
         cmp      al,typeVW               ;
         jnz      setup1b                 ;
setup1a: mov      al,'V'                  ;
         call     SendMouseCommand        ;
setup1b: mov      buttons,2               ;
         mov      colRange,1              ;
         mov      rowRange,1              ;
         push     portMI.ratState         ;
         mov      bx,offset portMI        ;
         call     SetSerialState          ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      al,mouse                ;
         cmp      al,typeMW               ;
         jz       setup1c                 ;
         cmp      al,typeVW               ;
         jnz      setup90                 ;
setup1c: mov      al,'V'                  ;
         call     SendMouseCommand        ;
         jmp      setup90                 ;

setup2:  cmp      al,proto5B              ;
         jnz      setup3                  ; Protocol 5B selected, set the
         mov      si,offset btnMap5B      ;  correct baud rate and program
         mov      al,mouse                ;  the mouse
         cmp      al,typeC                ;
         jnz      setup2a                 ;
         mov      bx,offset stup5B        ;
         call     SendMouseString         ;
         jmp      short setup2c           ;
setup2a: cmp      al,typeMW               ;
         jz       setup2b                 ;
         cmp      al,typeVW               ;
         jnz      setup2c                 ;
setup2b: mov      al,'U'                  ;
         call     SendMouseCommand        ;
setup2c: mov      buttons,3               ;
         mov      colRange,1              ;
         xor      rowRvFlg,0ffffh         ;
         mov      rowRange,1              ;
         push     port5B.ratState         ;
         mov      bx,offset port5B        ;
         call     SetSerialState          ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      al,mouse                ;
         cmp      al,typeC                ;
         jnz      setup2d                 ;
         mov      bx,offset stup5B        ;
         call     SendMouseString         ;
         jmp      setup90                 ;
setup2d: cmp      al,typeMW               ;
         jz       setup2e                 ;
         cmp      al,typeVW               ;
         jnz      setup90                 ;
setup2e: mov      al,'U'                  ;
         call     SendMouseCommand        ;
         jmp      setup90                 ;

setup3:  cmp      al,protoMM              ;
         jnz      setup4                  ; Protocol MM selected, set the
         cmp      mouse,typeSG            ;  correct baud rate and program
         jz       setup3a                 ;  the mouse
         cmp      mouse,typeSX            ;
         jz       setup3a                 ;
         cmp      mouse,typeSM            ;
         jz       setup3a                 ;
         cmp      mouse,typeAC            ;
         jnz      setup3b                 ;
setup3a: mov      si,offset btnMapMR      ;
         push     portMR.ratState         ;
         mov      bx,offset portMR        ;
         call     SetSerialState          ;
         mov      bx,offset stupSG        ;
         call     SendMouseString         ;
         jmp      short setup3d           ;
setup3b: cmp      mouse,typeC             ;
         jnz      setup3c                 ;
         mov      bx,offset stupMM        ;
         call     SendMouseString         ;
setup3c: mov      si,offset btnMapMM      ;
         push     portMM.ratState         ;
         mov      bx,offset portMM        ;
         call     SetSerialState          ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
setup3d: mov      buttons,3               ;
         xor      colRvFlg,0ffffh         ;
         mov      colRange,1              ;
         mov      rowRange,1              ;
         cmp      mouse,typeC             ;
         jnz      setup90                 ;
         mov      bx,offset stupMM        ;
         call     SendMouseString         ;
         jmp      setup90                 ;

setup4:  cmp      al,protoRE              ;
         jnz      setup5                  ; Protocol RE selected, set the
         mov      si,offset btnMapRE      ;  correct baud rate and program
         cmp      mouse,typeC             ;  the mouse
         jnz      setup4a                 ;
         mov      bx,offset stupRE        ;
         call     SendMouseString         ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
setup4a: mov      buttons,3               ;
         mov      colRange,1              ;
         xor      rowRvFlg,0ffffh         ;
         mov      rowRange,1              ;
         push     portRE.ratState         ;
         mov      bx,offset portRE        ;
         call     SetSerialState          ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         cmp      mouse,typeC             ;
         jnz      setup90                 ;
         mov      bx,offset stupRE        ;
         call     SendMouseString         ;
         jmp      setup90                 ;

setup5:  cmp      al,protoHA              ; Protocol HA selected, set the
         jnz      setup6                  ;  correct baud rate and program
         mov      si,offset btnMapHA      ;  the mouse
         mov      buttons,3               ;
         xor      rowRvFlg,0ffffh         ;
         push     portHA.ratState         ;
         mov      bx,offset portHA        ;
         call     SetSerialState          ;
         push     dx                      ;
         mov      bx,7500                 ; Set default range to [0..7500)
         mov      dx,7500                 ;  for both X and Y and default
         xor      cx,cx                   ;  MARGIN to zero
         call     SetDefaultRange         ;
         pop      dx                      ;
         jmp      setup90                 ;

setup6:  cmp      al,protoHR              ; Protocol HR selected, set the
         jnz      setup7                  ;  correct baud rate and program
         mov      si,offset btnMapHR      ;  the mouse
         mov      buttons,3               ;
         xor      rowRvFlg,0ffffh         ;
         push     portHR.ratState         ;
         mov      bx,offset portHR        ;
         call     SetSerialState          ;
         push     dx                      ;
         mov      bx,7500                 ; Set default range to [0..7500)
         mov      dx,7500                 ;  for both X and Y and default
         xor      cx,cx                   ;  MARGIN to zero
         call     SetDefaultRange         ;
         pop      dx                      ;
         jmp      setup90                 ;

setup7:  cmp      al,proto3B              ; Protocol 3B selected, set the
         jnz      setup8                  ;  correct baud rate and program
         mov      si,offset btnMap3B      ;  the mouse
         mov      buttons,3               ;
         mov      colRange,1              ;
         xor      rowRvFlg,0ffffh         ;
         mov      rowRange,1              ;
         push     port3B.ratState         ;
         mov      bx,offset port3B        ;
         call     SetSerialState          ;
         jmp      setup90                 ;

setup8:  cmp      al,protoBS              ;
         jnz      setup9                  ;
         add      dx,busInit              ;
         mov      al,busSetup             ;
         out      dx,al                   ; Initialize buss mouse
         mov      deviceData.mouseType,busMouse
         mov      si,offset btnMapBS      ;
         mov      buttons,3               ;
         mov      colRange,1              ;
         mov      rowRange,1              ;
         jmp      setup91                 ;

setup9:  cmp      al,protoIN              ;
         jnz      setup10                 ; Initialize Inport buss mouse
         mov      ax,rprtRate             ;
         call     SetInportRate           ;
         mov      deviceData.mouseType,busMouse
         mov      si,offset btnMapIN      ;
         mov      buttons,3               ;
         mov      colRange,1              ;
         mov      rowRange,1              ;
         jmp      setup91                 ;

setup10: cmp      al,protoMA              ;
         jnz      setup11                 ; SummaGraphic's MM (absolute)
         mov      si,offset btnMapMA      ;  protocol MA selected, set the
         mov      buttons,3               ;  correct baud rate and program
         xor      rowRvFlg,0ffffh         ;
         push     portMA.ratState         ;  the mouse
         mov      bx,offset portMA        ;
         call     SetSerialState          ;
         push     dx                      ;
         mov      bx,6000                 ; Set default range to [0..6000)
         mov      dx,6000                 ;  for both X and Y and default
         xor      cx,cx                   ;  MARGIN to zero
         call     SetDefaultRange         ;
         pop      dx                      ;
         mov      bx,offset stupMA        ;
         call     SendMouseString         ;
         jmp      setup90                 ;

setup11: cmp      al,protoMR              ; SummaGraphic's MM (relative)
         jnz      setup12                 ;  protocol MR selected, set the
         mov      si,offset btnMapMR      ;  correct baud rate and program
         mov      buttons,3               ;  the mouse
         xor      rowRvFlg,0ffffh         ;
         push     portMR.ratState         ;
         mov      bx,offset portMR        ;
         call     SetSerialState          ;
         push     dx                      ;
         mov      bx,6000                 ; Set default range to [0..6000)
         mov      dx,6000                 ;  for both X and Y and default
         xor      cx,cx                   ;  MARGIN to zero
         call     SetDefaultRange         ;
         pop      dx                      ;
         mov      bx,offset stupMR        ;
         call     SendMouseString         ;
         jmp      setup90                 ;

setup12: cmp      al,protoUA              ;
         jnz      setup13                 ; SummaGraphic's UIOF (absolute)
         mov      si,offset btnMapUA      ;  protocol UA selected, set the
         mov      buttons,3               ;  correct baud rate and program
         xor      rowRvFlg,0ffffh         ;  the mouse
         push     portUA.ratState         ;
         mov      bx,offset portUA        ;
         call     SetSerialState          ;
         push     dx                      ;
         mov      bx,12000                ; Set default range to [0..12000)
         mov      dx,12000                ;  for both X and Y and default
         xor      cx,cx                   ;  MARGIN to zero
         call     SetDefaultRange         ;
         pop      dx                      ;
         mov      bx,offset stupUA        ;
         call     SendMouseString         ;
         jmp      setup90                 ;

setup13: cmp      al,protoUR              ; SummaGraphic's UIOF (relative)
         jnz      setup14                 ;  protocol UR selected, set the
         mov      si,offset btnMapUR      ;  correct baud rate and program
         mov      buttons,3               ;  the mouse
         xor      rowRvFlg,0ffffh         ;
         push     portUR.ratState         ;
         mov      bx,offset portUR        ;
         call     SetSerialState          ;
         push     dx                      ;
         mov      bx,12000                ; Set default range to [0..12000)
         mov      dx,12000                ;  for both X and Y and default
         xor      cx,cx                   ;  MARGIN to zero
         call     SetDefaultRange         ;
         pop      dx                      ;
         mov      bx,offset stupUR        ;
         call     SendMouseString         ;
         jmp      setup90                 ;

setup14: cmp      al,protoFA              ;
         jnz      setup15                 ; Felix (absolute) protocol FA
         mov      si,offset btnMapFA      ;  selected, set the correct baud
         mov      buttons,3               ;  rate and program the mouse
         push     portFA.ratState         ;
         mov      bx,offset portFA        ;
         call     SetSerialState          ;
         push     dx                      ;
         cmp      response,1              ;
         jnc      setup14a                ;
         mov      bx,640                  ; Set default range to [0..640)
         mov      dx,640                  ;  for both X and Y and default
         xor      cx,cx                   ;  MARGIN to zero
         call     SetDefaultRange         ;
         pop      dx                      ;
         jmp      setup90                 ;
setup14a:mov      bx,640                  ; Set default range to [0..640)
         mov      dx,640                  ;  for both X and Y and default
         mov      cx,4                    ;  MARGIN to 4
         call     SetDefaultRange         ;
         mov      cl,response             ;
         mov      ax,rowRange             ; If Felix is to be accelerated
         shl      ax,cl                   ;  then increase range and set
         sub      ax,rowRange             ;  maximum base for both row and
         mov      rowBmax,ax              ;  column
         add      rowRange,ax             ;
         mov      ax,colRange             ;
         shl      ax,cl                   ;
         sub      ax,colRange             ;
         mov      colBmax,ax              ;
         add      colRange,ax             ;
         pop      dx                      ;
         jmp      setup90                 ;

setup15: cmp      al,protoFR              ; Felix (relative) protocol FR
         jnz      setup16                 ;  selected, set the correct baud
         mov      si,offset btnMapFR      ;  rate and program the mouse
         mov      buttons,3               ;
         xor      colRvFlg,0ffffh         ;
         xor      rowRvFlg,0ffffh         ;
         push     portFR.ratState         ;
         mov      bx,offset portFR        ;
         call     SetSerialState          ;
         push     dx                      ;
         mov      bx,640                  ; Set default range to [0..640)
         mov      dx,640                  ;  for both X and Y and default
         mov      cx,4                    ;  MARGIN to 4
         call     SetDefaultRange         ;
         pop      dx                      ;
         jmp      setup90                 ;

setup16: cmp      al,protoTA              ;
         jnz      setup17                 ; UnMouse (absolute) protocol TA
         mov      buttons,2               ;  selected, set the correct baud
         xor      rowRvFlg,0ffffh         ;  rate and program the mouse
         push     portTA.ratState         ;
         mov      bx,offset portTA        ;
         call     SetSerialState          ;
         push     dx                      ;
         mov      bx,1024                 ; Set default range to [0..1024)
         mov      dx,1024                 ;  for both X and Y and default
         xor      cx,cx                   ;  MARGIN to 0
         call     SetDefaultRange         ;
         pop      dx                      ;
         mov      bx,offset stupTA1       ;
         call     SendMouseString         ; Send initialization strings,
         mov      cx,250                  ;  ignore any response but give the
         mov      si,8                    ;  mouse enough time to handle the
         call     ReadMouseResponse       ;  commands
         mov      bx,offset stupTA2       ;
         call     SendMouseString         ;
         mov      cx,250                  ;
         mov      si,8                    ;
         call     ReadMouseResponse       ;
         mov      si,offset btnMapTA      ;
         jmp      setup90                 ;

setup17: cmp      al,protoTR              ; UnMouse (relative) protocol TR
         jnz      setup18                 ;  selected, set the correct baud
         mov      buttons,2               ;  rate and program the mouse
         xor      colRvFlg,0ffffh         ;
         push     portTR.ratState         ;
         mov      bx,offset portTR        ;
         call     SetSerialState          ;
         push     dx                      ;
         mov      bx,1024                 ; Set default range to [0..1024)
         mov      dx,1024                 ;  for both X and Y and default
         xor      cx,cx                   ;  MARGIN to 0
         call     SetDefaultRange         ;
         pop      dx                      ;
         mov      bx,offset stupTR1       ;
         call     SendMouseString         ; Send initialization strings,
         mov      cx,250                  ;  ignore any response but give the
         mov      si,8                    ;  mouse enough time to handle the
         call     ReadMouseResponse       ;  commands
         mov      bx,offset stupTR2       ;
         call     SendMouseString         ;
         mov      cx,250                  ;
         mov      si,8                    ;
         call     ReadMouseResponse       ;
         mov      si,offset btnMapTR      ;
         jmp      setup90                 ;

setup18: cmp      al,protoBA              ;
         jnz      setup19                 ; Bit Pad One/Two (absolute)
         mov      si,offset btnMapBA      ;  protocol BA selected, set the
         xor      rowRvFlg,0ffffh         ;  correct baud rate and program
         push     portBA.ratState         ;  the mouse
         mov      bx,offset portBA        ;
         call     SetSerialState          ;
         push     dx                      ;
         mov      bx,2400                 ; Set default range to [0..2400)
         mov      dx,2400                 ;  for both X and Y and default
         xor      cx,cx                   ;  MARGIN to 0
         call     SetDefaultRange         ;
         pop      dx                      ;
         mov      bx,offset stupBA        ;
         call     SendMouseString         ;
         jmp      setup90                 ;

setup19: cmp      al,protoBR              ; Bit Pad One/Two (relative)
         jnz      setup20                 ;  protocol BR selected, set the
         mov      si,offset btnMapBR      ;  correct baud rate and program
         xor      colRvFlg,0ffffh         ;  the mouse
         push     portBR.ratState         ;
         mov      bx,offset portBR        ;
         call     SetSerialState          ;
         push     dx                      ;
         mov      bx,2400                 ; Set default range to [0..2400)
         mov      dx,2400                 ;  for both X and Y and default
         xor      cx,cx                   ;  MARGIN to 0
         call     SetDefaultRange         ;
         pop      dx                      ;
         mov      bx,offset stupBR        ;
         call     SendMouseString         ;
         jmp      setup90                 ;

setup20: cmp      al,protoCA              ;
         jnz      setup21                 ; Bit Pad CR (absolute) protocol
         mov      si,offset btnMapCA      ;  selected, set the correct baud
         xor      rowRvFlg,0ffffh         ;  rate and program the mouse
         push     portCA.ratState         ;
         mov      bx,offset portCA        ;
         call     SetSerialState          ;
         push     dx                      ;
         mov      bx,2400                 ; Set default range to [0..2400)
         mov      dx,2400                 ;  for both X and Y and default
         xor      cx,cx                   ;  MARGIN to 0
         call     SetDefaultRange         ;
         pop      dx                      ;
         mov      bx,offset stupCA        ;
         call     SendMouseString         ;
         jmp      setup90                 ;

setup21: cmp      al,protoCR              ; Bit Pad CR (relative) protocol
         jnz      setup22                 ;  selected, set the correct baud
         mov      si,offset btnMapCR      ;  rate and program the mouse
         xor      colRvFlg,0ffffh         ;
         push     portCR.ratState         ;
         mov      bx,offset portCR        ;
         call     SetSerialState          ;
         push     dx                      ;
         mov      bx,2400                 ; Set default range to [0..2400)
         mov      dx,2400                 ;  for both X and Y and default
         xor      cx,cx                   ;  MARGIN to 0
         call     SetDefaultRange         ;
         pop      dx                      ;
         mov      bx,offset stupCR        ;
         call     SendMouseString         ;
         jmp      setup90                 ;

setup22: cmp      al,protoPS              ; PS/2 mouse protocol selected
         jnz      setup23                 ;
         call     DisableKeyboardIrq      ;
         mov      al,ps2DsKey             ; Disable keyboard to avoid
         call     SendPs2Control          ;  interference
         mov      al,pixEnabl             ;
         call     SendPs2Command          ; Enable mouse and set reporting
         mov      ax,rprtRate             ;  rate
         call     SetPs2Rate              ;
         call     GetCommandByte          ; Set new command byte with the
         and      al,not (ps2AuxDs+ps2KeyDs+ps2AuxEn+ps2KeyEn)
         or       al,ps2AuxEn+ps2KeyEn    ;  auxilary interrupt enabled
         call     PutCommandByte          ;
         call     EnableKeyboardIrq       ;
         mov      deviceData.mouseType,ps2Mouse
         mov      colRange,1              ;
         mov      rowRange,1              ;
         xor      rowRvFlg,0ffffh         ;
         mov      si,offset btnMapPS      ;
         cmp      ps2Mask,0               ;
         jnz      setup92                 ;
         movzx    ax,deviceData.numButt   ;
         mov      bx,sync4Bit             ; Determine the actual sync flags
         bts      bx,ax                   ;  based on the actual number of
         mov      ps2Mask,bl              ;  buttons
         jmp      setup92                 ;

setup23: cmp      al,protoNM              ; Provide "no mouse" support
         jnz      setup95                 ;
         jmp      setup91                 ;

setup90: pop      ax                      ;
         cmp      baudRate,0              ; If no baud rate specified, then
         jnz      setup91                 ;  use default baud rate for
         mov      baudRate,ax             ;  protocol

setup91: mov      al,buttons              ;
         cmp      deviceData.numButt,al   ; Allow protocol to restrict the
         jc       setup92                 ;  number of buttons available
         mov      deviceData.numButt,al   ;

setup92: cmp      protocol,protoNM        ;
         jz       setup95                 ;

         mov      ah,deviceData.numButt   ;
         sub      ah,2                    ; Set up index = 00xx for 2 button
         mov      di,offset intrMap       ;  translate and 01xx for 3 button
         movzx    cx,byte ptr [si]        ;
setup93: inc      si                      ; Move the button translate map
         movzx    bx,byte ptr [si]        ;  needed by the interrupt handler
         mov      bl,btnOrder[bx]         ;
         mov      bh,ah                   ; Reorder physical buttons and
         mov      al,btnMap2[bx]          ;  replace the button presses by
         mov      byte ptr [di],al        ;  the appropriate OS/2 button
         inc      di                      ;  presses
         loop     setup93                 ;

         mov      ax,colRvFlg             ;
         and      ax,colRange             ; Partially precompute value for...
         mov      colRvVal,ax             ;  If (flag)
         mov      ax,rowRvFlg             ;        x <- N - x
         and      ax,rowRange             ;      Else
         mov      rowRvVal,ax             ;        x <- x

         cmp      protocol,protoBS        ;
         jz       setup95                 ; If not a serial protocol then
         cmp      protocol,protoIN        ;  skip serial related code
         jz       setup95                 ;
         cmp      protocol,protoPS        ;
         jz       setup95                 ;

         mov      ax,baudRate             ;
         call     SetMouseBaudRate        ; Set mouse and uart to selected
         mov      ax,baudRate             ;  baud rate
         call     SetBaudRate             ;

         cmp      buffered,0              ; Enable FIFO buffer if buffering
         jz       setup94                 ;  is active
         mov      al,fcrTgr01+fcrXmit+fcrRcvr+fcrBfrd
         add      dx,fcr                  ;
         out      dx,al                   ;
         MyIoDelay                        ;
         sub      dx,fcr                  ;

setup94: mov      cx,20000                ; Start with a clean slate
         call     EatMouseGarbage         ;
         add      dx,mcr                  ;
         mov      al,mcrOut2+mcrDtr+mcrRts; Enable interrupts from mouse
         out      dx,al                   ;
         MyIoDelay                        ;

setup95: ret                              ;

SetupDevice endp

cseg     ends



;
; Interface:
;
;    SetDefaultRange(row bx, column dx, margin cx)
;
; Description:
;
;    Sets the default range for absolute devices.  If either the user has
; specified the range or it has been determined by interrogating the
; hardware then that value is used, otherwise the passed default value is
; used.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

SetDefaultRange proc near

         cmp      rowMax,0                ;
         jnz      sdr1                    ;
         mov      rowMax,bx               ;
sdr1:    cmp      colMax,0                ;
         jnz      sdr2                    ; If there is no surface size then
         mov      colMax,dx               ;  set default values for X, Y and
sdr2:    mov      ax,colMargn             ;  and MARGIN
         or       ax,rowMargn             ;
         jnz      sdr3                    ;
         mov      colMargn,cx             ;
         mov      rowMargn,cx             ;
sdr3:    mov      ax,rowMin               ;
         cmp      ax,rowMax               ;  If the row or column minimum
         jc       sdr4                    ;   point returned is not less than
         mov      rowMin,0                ;   the maximum, set it to zero
sdr4:    mov      ax,colMin               ;
         cmp      ax,colMax               ;
         jc       sdr5                    ;
         mov      colMin,0                ;
sdr5:    mov      ax,rowMax               ;
         sub      ax,rowMin               ; Determine the row and column
         mov      rowRange,ax             ;  range by subtracting the
         mov      ax,rowMargn             ;  respective minimum from its
         add      ax,ax                   ;  maximum.
         cmp      ax,rowRange             ;
         jnc      sdr6                    ; If the respective margins are
         sub      rowRange,ax             ;  larger than the available area
         shr      ax,1                    ;  set the margins to zero
         add      rowMin,ax               ;
         sub      rowMax,ax               ; Otherwise increase the minimums
sdr6:    mov      ax,colMax               ;  and reduce the maximums by the
         sub      ax,colMin               ;  respective margin
         mov      colRange,ax             ;
         mov      ax,colMargn             ;
         add      ax,ax                   ;
         cmp      ax,colRange             ;
         jnc      sdr7                    ;
         sub      colRange,ax             ;
         shr      ax,1                    ;
         add      colMin,ax               ;
         sub      colMax,ax               ;
sdr7:    ret                              ;

SetDefaultRange endp

cseg     ends



;
; Interface:
;
;    GetIrq()
;
; Description:
;
;    Determines interrupt to be used, sets up 8259 controller to be used
; and 8259 interrupt mask.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Registers ax, bx and dx are altered.
;

cseg     segment

         align    4

GetIrq proc near

         cmp      protocol,protoNM        ;
         jz       girqDone                ;
         mov      cl,deviceData.irq       ;
         or       cl,cl                   ; Get the irq number, if none then
         jnz      girq1                   ;  calculate irq number from high
         mov      ax,deviceData.portAddr  ;  byte of port address -- this is
         mov      cl,ah                   ;  not a really good method, but
         and      cl,0fh                  ;  works for COM1 and COM2 and
         inc      cl                      ;  sometimes for COM3 and COM4
girq1:   cmp      cl,2                    ;
         jnz      girq2                   ; If irq 2 then replace
         mov      cl,9                    ;  with irq 9 because slave
girq2:   mov      deviceData.irq,cl       ;  8259 uses irq 2 and the
         mov      dx,mstr8259             ;  hardware maps 2 into 9
         cmp      cl,8                    ;
         jc       girq3                   ; Determine 8259 to use,
         mov      dx,slav8259             ;  irqs 0..7  = master
girq3:   mov      mskPrt8259,dx           ;  irqs 8..15 = slave
         mov      ax,1                    ;
         and      cl,07h                  ; Build 8259 mask from irq number
         shl      ax,cl                   ;
         mov      disable8259,al          ;
         not      al                      ; Save irq enable/disable masks
         mov      enable8259,al           ;
girqDone:ret                              ;

GetIrq endp

cseg     ends



;
; Interface:
;
;    GetIrqOwnership()
;
; Description:
;
;    Requests interrupt ownership from OS/2.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Registers ax, bx, cx, dx, si, di and es are altered.
;

dseg     segment

gioFix   db       0,1,7,17

gioTbl2  db       -13,-11, -9, -7,-5,-3,-1,0,1,3,5, 7, 9,11,13
gioTbl4  db       -21,-17,-15,-11,-7,-3,-1,0,1,3,7,11,15,17,21
gioTbl8  db       -39,-31,-23,-15,-7,-3,-1,0,1,3,7,15,23,31,39

dseg     ends

cseg     segment

         align    4

GetIrqOwnership proc near

         mov      bl,protocol                   ;

         mov      ax,offset InterruptHandlerMP  ; Use the requested protocol
         cmp      bl,protoMP                    ;  to select the interrupt
         jz       gioSet                        ;  handler to be used

         mov      ax,offset InterruptHandlerMI  ;
         cmp      bl,protoMI                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandler3B  ;
         cmp      bl,proto3B                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandler5B  ;
         cmp      bl,proto5B                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerMM  ;
         cmp      bl,protoMM                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerRE  ;
         cmp      bl,protoRE                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerHA  ;
         cmp      bl,protoHA                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerHR  ;
         cmp      bl,protoHR                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerMA  ;
         cmp      bl,protoMA                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerMR  ;
         cmp      bl,protoMR                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerFA  ;
         cmp      bl,protoFA                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerFR  ;
         cmp      bl,protoFR                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerBS  ;
         cmp      bl,protoBS                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerIN  ;
         cmp      bl,protoIN                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerPS  ;
         cmp      bl,protoPS                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerTA  ;
         cmp      bl,protoTA                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerTR  ;
         cmp      bl,protoTR                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerUA  ;
         cmp      bl,protoUA                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerUR  ;
         cmp      bl,protoUR                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerBA  ;
         cmp      bl,protoBA                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerBR  ;
         cmp      bl,protoBR                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerCA  ;
         cmp      bl,protoCA                    ;
         jz       gioSet                        ;

         mov      ax,offset InterruptHandlerCR  ;
         cmp      bl,protoCR                    ;
         jz       gioSet                        ;

         cmp      bl,protoNM                    ;
         jz       gioDone                       ;

         stc                                    ;
         ret                                    ;

gioSet:  xor      bh,bh                         ;
         mov      bl,deviceData.irq             ; Get interrupt ownership
         mov      dl,DevHlp_SetIRQ              ;  from OS/2 -- but we're
         cmp      shared,0                      ;  willing to share if the
         jnz      gioSet1                       ;  SHARE option is used
         mov      dh,01h                        ;
         call     deviceHelp                    ; If an error, then try
         jnc      gioAccl1                      ;  again -- but unshared
gioSet1: xor      dh,dh                         ;  this time
         call     deviceHelp                    ;

gioAccl1:mov      al,response                   ;
         or       al,al                         ; Set pointer to acceleration
         jz       gioDone                       ;  or deceleration module
         jns      gioAccl2                      ;
         neg      al                            ;
         mov      reshift,al                    ;
         mov      accelrat,offset DeAccelerate  ;
         clc                                    ;
         ret                                    ;
gioAccl2:mov      reshift,al                    ;
         movzx    bx,al                         ;
         mov      bl,gioFix[bx]                 ;
         mov      accelFix,bx                   ;
         mov      si,offset gioTbl2             ;
         cmp      al,1                          ;
         jz       gioAccl3                      ;
         mov      si,offset gioTbl4             ;
         cmp      al,2                          ;
         jz       gioAccl3                      ;
         mov      si,offset gioTbl8             ;
gioAccl3:mov      di,offset accelTbl            ;
         mov      ax,ds                         ;
         mov      es,ax                         ;
         mov      cx,15                         ;
     rep movsb                                  ;
         mov      accelrat,offset Accelerate    ;
gioDone: clc                                    ;
         ret                                    ;

GetIrqOwnership endp

cseg     ends



;
; Interface:
;
;    GetPortOwnership()
;
; Description:
;
;    Requests port ownership from BIOS and ABIOS.  This is only done for
; PS/2 mice and for serial mice if the port address if for COM1..COM4,
; other port addresses are not known to BIOS and ABIOS.
;
; Side effects:
;
;    Registers ax, bx, dx, di and es are altered.
;

cseg     segment

         align    4

GetPortOwnership proc near

         cmp      protocol,protoBS           ;
         jz       gpoDone                    ; Check type of device, if not
         cmp      protocol,protoIN           ;  serial or PS/2 then nothing
         jz       gpoDone                    ;  to do, otherwise get port
         cmp      protocol,protoPS           ;  ownership
         jz       gpoPs2                     ;
         cmp      protocol,protoNM           ;
         jz       gpoDone                    ;

         mov      ax,deviceData.portAddr     ;
         or       ax,ax                      ;
         jz       gpoDone                    ;
         mov      di,biosData                ;
         mov      es,di                      ; Search BIOS serial port list
         xor      di,di                      ;  for mouse port
         mov      bl,1                       ;
gpoLoop: cmp      ax,word ptr es:[di]        ;
         jz       gpoBios                    ;
         add      di,2                       ;
         inc      bl                         ;
         cmp      bl,4                       ;
         jbe      gpoLoop                    ;
         ret                                 ;

gpoBios: mov      pComAddr,di                ;
         mov      deviceData.comPortNum,bl   ;
         mov      word ptr es:[di],0         ; Zero port address in

         cmp      isAbios,1                  ;
         jnz      gpoDone                    ; If ABIOS present then request
         mov      al,06h                     ;  BIOS area to reserve
         xor      dh,dh                      ;  serial port
         mov      dl,DevHlp_GetLIDEntry      ;
         call     deviceHelp                 ; If running on ABIOS
         jc       gpoDone                    ;  machine, then allocate
         mov      gotLid,1                   ;  serial port from ABIOS
         mov      devLid,ax                  ;
         ret                                 ;

gpoPs2:  cmp      isAbios,1                  ;
         jnz      gpoDone                    ; If ABIOS present then request
         mov      al,0bh                     ;  device ownership from ABIOS
         mov      bl,1                       ;
         xor      dh,dh                      ;
         mov      dl,DevHlp_GetLIDEntry      ;
         call     DeviceHelp                 ;
         jc       gpoDone                    ;
         mov      gotLid,1                   ;
         mov      devLid,ax                  ;
gpoDone: ret                                 ;

GetPortOwnership endp

cseg     ends



;
; Interface:
;
;    FindMouse()
;
; Description:
;
;    Finds the mouse.  This determines type type of mouse, its protocol, the
; port address and the irq to be used.  When searching for serial mice, if a
; port address is explicitly given using the COM=# option then it is used,
; otherwise the list of serial port addresses in the BIOS is searched for a
; mouse.  If a mouse is found then basic initialization is performed as a
; side effect.
;
; Normal exit:
;
;    Carry clear - mouse found and basic initialization performed.
;
; Error exit:
;
;    Carry set -- mouse not found.
;
; Side effects:
;
;    Registers ax, bx, cx, dx and si are altered.
;

cseg     segment

         align    4

FindMouse proc near

         push     di                      ;
         push     es                      ;
         call     CheckMouseTypeX         ;
         jnc      gptBut                  ;
         call     FindInportMouse         ;
         jnc      gptBut                  ;
         call     FindBussMouse           ;
         jnc      gptBut                  ;
         call     FindPs2Mouse            ;
         jnc      gptBut                  ;
         call     CheckMouseTypeNM        ;
         jnc      gptBut                  ;
         mov      dx,deviceData.portAddr  ;
         or       dx,dx                   ; If serial port explicitly given
         jz       gptHunt                 ;  then check it
         call     FindSerialMouse         ;
         jc       gptExit                 ;
gptOk:   mov      deviceData.portAddr,dx  ;
         mov      al,uartType             ;
         cmp      al,ns16550a             ; ns16550a or above may be
         jnc      gptBut                  ;  buffered
         mov      buffered,0              ;
gptBut:  mov      al,buttons              ; Save number of buttons if it
         or       al,al                   ;  does not increase the current
         jz       gptDone                 ;  number and is not zero
         cmp      deviceData.numButt,al   ;
         jc       gptDone                 ;
         mov      deviceData.numButt,al   ;
gptDone: clc                              ;
         pop      es                      ; Return success
         pop      di                      ;
         ret                              ;
gptHunt: mov      ax,biosData             ; Hunt through the BIOS serial
         mov      es,ax                   ;  port list for mouse port
         xor      di,di                   ;
gptLoop: mov      dx,word ptr es:[di]     ;
         call     FindSerialMouse         ;
         jnc      gptOk                   ;
         add      di,2                    ;
         cmp      di,8                    ;
         jc       gptLoop                 ;
         stc                              ;
gptExit: pop      es                      ;
         pop      di                      ;
         ret                              ;

FindMouse endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeX(portAddr dx)
;
; Description:
;
;    Checks for type X mouse.
;
; Normal exit:
;
;    Carry clear.  Mouse found -- this only requires that the user specify
; MOUSE=* and PROTOCOL=xx for some valid xx.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;

cseg     segment

         align    4

CheckMouseTypeX proc near

         mov      al,mouse                ;
         mov      ah,protocol             ; Type X mouse must be specified
         cmp      al,typeX                ;  by user, as must the protocol.
         jnz      cmtx1                   ;  This also implies that the port
         or       ah,ah                   ;  to be used must be specified by
         jz       cmtx1                   ;  the user.  Default to 3 buttons.
         or       mseSpeed,mse1200+mse2400+mse4800+mse9600
         or       mseRate,rate100+rate200 ;
         mov      buttons,3               ;
         ret                              ;
cmtx1:   stc                              ;
         ret                              ;

CheckMouseTypeX endp

cseg     ends



;
; Interface:
;
;    FindInportMouse()
;
; Description:
;
;    Checks for the existence of a properly configured Inport buss mouse.
; The buss mouse interrupts will be disabled on exit.
;
; Normal exit:
;
;    Carry clear -- Inport buss mouse found.
;
; Error exit:
;
;    Carry set -- Inport buss mouse not found.
;
; Side effects:
;
;    Stack is clean, registers ax, bx, cx, dx and si are altered.
;

cseg     segment

         align    4

FindInportMouse proc near

         mov      al,mouse                ;
         cmp      al,typeS                ; If mouse type specified and NOT a
         jz       fipm4                   ;  Inport buss mouse then skip
         or       al,al                   ;  check, if mouse specified then
         jz       fipm1                   ;  check anyway  because we have
         cmp      al,typeI                ;  to find the irq.
         jnz      fipm4                   ;
         mov      mouse,typeErr           ;
fipm1:   mov      dx,inpPrim              ;
         call     CheckMouseTypeI         ; Check for presence of Inport
         jnc      fipm2                   ;  mouse at both primary and
         mov      dx,inpSec               ;  secondary addresses
         call     CheckMouseTypeI         ;
         jc       fipm5                   ;
fipm2:   mov      bl,inpMastr             ; Now determine the interrupt,
         mov      si,mstr8259             ;  check for interrupts on the
         call     FindInportInterrupt     ;  master 8259 first, if not there
         jnc      fipm3                   ;  then check the slave 8259.  If
         mov      bl,inpSlave             ;  found there then adjust irq
         mov      si,slav8259             ;  number, otherwise no Inport
         call     FindInportInterrupt     ;  mouse present AND properly
         jc       fipm5                   ;  configured
         add      al,8                    ;
fipm3:   mov      deviceData.mouseType,inpMouse
         mov      deviceData.portAddr,dx  ;
         mov      deviceData.irq,al       ;
         or       mseRate,rate30+rate50+rate100+rate200
         mov      buttons,3               ;
         mov      mouse,typeI             ; Type I mouse found
         mov      protocol,protoIN        ;
         ret                              ;
fipm4:   stc                              ;
fipm5:   ret                              ;

FindInportMouse endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeI(portAddr dx)
;
; Description:
;
;    Checks for type I mouse.
;
; Normal exit:
;
;    Carry clear.  Mouse found.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax and cx are altered.
;

cseg     segment

         align    4

CheckMouseTypeI proc near

         push     dx                      ;
         add      dx,inpId                ; Check Inport id register for
         in       al,dx                   ;  Inport Id Signature byte.  This
         cmp      al,inPortId             ;  value alternates between the
         jz       cmti1                   ;  signature byte and the version
         MyIoDelay                        ;  byte every other read, so try
         in       al,dx                   ;  twice
         cmp      al,inPortId             ;
         jnz      cmti3                   ;
cmti1:   MyIoDelay                        ; Last read was signature byte, so
         in       al,dx                   ;  read version/revision byte and
         mov      ah,al                   ;  save for comparision
         mov      cx,16                   ;
cmti2:   MyIoDelay                        ; Read the signature byte and the
         in       al,dx                   ;  version byte several times to
         cmp      al,inPortId             ;  make sure that the Inport
         jnz      cmti3                   ;  interface is really present.
         MyIoDelay                        ;
         in       al,dx                   ;
         cmp      al,ah                   ;
         loopz    cmti2                   ;
         jz       cmti4                   ;
cmti3:   stc                              ;
cmti4:   pop      dx                      ;
         ret                              ;

CheckMouseTypeI endp

cseg     ends



;
; Interface:
;
;    FindInportInterrupt(portAddr dx, 8259Addr si, irqLevels bl)
;
; Description:
;
;    Finds the interrupt for the Inport buss mouse.  Returns the irq number
; in al.
;
; Normal exit:
;
;    Carry clear -- interrupt found (and in al).
;
; Error exit:
;
;    Carry set -- interrupt not found.
;
; Side effects:
;
;    Stack is clean, registers ax, bx and cx are altered.
;
; Note: The basic approach used is simply to turn the Inport interrupt line
;       off & on repeatedly, and VERY rapidly (compared to normal interrupt
;       rates).  The interrupt(s) tied to the Inport will follow the toggling.
;       It is POSSIBLE, but VERY unlikely for any OTHER interrupt to also
;       follow.  We expect exactly ONE such interrupt to exist.  None or more
;       than one is an error.  Further, while it is generally poor practice
;       to disable interrupts for an extended period of time, it is necessary
;       here to disable interrupts for the entire routine.
;

cseg     segment

         align    4

FindInportInterrupt  proc  near

         xchg     dx,si                   ;
         cli                              ;
         in       al,dx                   ; Save the current register mask
         MyIoDelay                        ;  and mask off the interrupt
         push     ax                      ;  levels we are going to check
         or       al,bl                   ;
         out      dx,al                   ;
         MyIoDelay                        ;
         dec      dx                      ; Now get 8259 control port and
         mov      al,irr8259              ;  select the interrupt request
         out      dx,al                   ;  register
         xchg     dx,si                   ;
         mov      al,inpReset             ;
         out      dx,al                   ; Reset Inport interface and select
         MyIoDelay                        ;  the mode register
         mov      al,inpMode              ;
         out      dx,al                   ;
         MyIoDelay                        ;
         add      dx,inpData - inpAddr    ;
         mov      ah,bl                   ; Set up to toggle irq repeatedly
         mov      cx,16                   ;
fipi1:   mov      al,inpTimrE + inpHz0I0  ;
         out      dx,al                   ; Turn irq off, and only keep
         MyIoDelay                        ;  interrupts which are now off and
         xchg     dx,si                   ;  part of our eligible irq mask
         in       al,dx                   ;
         and      al,bl                   ; We have a critical section where
         xor      al,bl                   ;  the selcted 8259 register cannot
         and      ah,al                   ;  be changed, so disable interrupts
         xchg     dx,si                   ;  during that period
         mov      al,inpTimrE + inpHz0I1  ;
         out      dx,al                   ; Now, turn irq on, and only keep
         MyIoDelay                        ;  interrupts which are now on and
         xchg     dx,si                   ;  part of our eligible irq mask
         in       al,dx                   ;
         and      al,bl                   ; Repeat toggle as long as there
         and      ah,al                   ;  are ANY possible interrupts
         xchg     dx,si                   ;  and while the loop counter is
         loopnz   fipi1                   ;  non-zero
         movzx    bx,ah                   ; Get active interrupts in bx
         mov      al,inpHz0I1             ;
         out      dx,al                   ; Disable Inport
         MyIoDelay                        ;
         add      dx,inpAddr - inpData    ;
         xchg     dx,si                   ; Leave 8259 isr selected because
         mov      al,isr8259              ;  that is hardware default
         out      dx,al                   ;
         MyIoDelay                        ;
         pop      ax                      ; Restore original 8259 mask (and
         inc      dx                      ;  hope nobody changed it on us)
         out      dx,al                   ;
         sti                              ;
         xchg     dx,si                   ;
         bsf      ax,bx                   ;
         jz       fipi2                   ; Get index of irq into ax, error
         btr      bx,ax                   ;  if none are set.  Then clear
         or       bx,bx                   ;  that bit, and if any more are
         jz       fipi3                   ;  set then error, otherwise we
fipi2:   stc                              ;  now have the irq # in ax
fipi3:   ret                              ;

FindInportInterrupt  endp

cseg     ends



;
; Interface:
;
;    FindBussMouse()
;
; Description:
;
;    Checks for the existence of a properly configured buss mouse.  The buss
; mouse interrupts will be disabled on exit.
;
; Normal exit:
;
;    Carry clear -- buss mouse found.
;
; Error exit:
;
;    Carry set -- buss mouse not found.
;
; Side effects:
;
;    Stack is clean, registers ax, bx, cx and dx are altered.
;

cseg     segment

         align    4

FindBussMouse proc near

         mov      al,mouse                ;
         cmp      al,typeS                ; If mouse type specified and NOT a
         jz       cmtb3                   ;  buss mouse then skip check, if
         or       al,al                   ;  mouse specified then check anyway
         jz       cmtb1                   ;  because we have to find the irq.
         cmp      al,typeB                ;
         jnz      cmtb3                   ;
         mov      mouse,typeErr           ;
cmtb1:   push     deviceData.portAddr     ;
         mov      dx,busBase              ;
         mov      deviceData.portAddr,dx  ;
         add      dx,busInit              ;
         mov      al,busSetup             ;
         out      dx,al                   ; Initialize buss mouse
         MyIODelay                        ;
         add      dx,busSig-busInit       ;
         mov      al,0a5h                 ; Check if we can write a signature
         out      dx,al                   ;  byte to the mouse interface and
         MyIODelay                        ;  get it back -- if not, then no
         in       al,dx                   ;  buss mouse
         MyIODelay                        ;
         cmp      al,0a5h                 ;
         jnz      cmtb2                   ;
         add      dx,busCtrl-busSig       ;
         call     FindBusInterrupt        ; Find the buss mouse interrupt
         jc       cmtb2                   ;
         mov      deviceData.irq,al       ;
         mov      deviceData.mouseType,busMouse
         xor      al,al                   ; Clear control port
         out      dx,al                   ;
         add      sp,2                    ; Discard original port address,
         mov      buttons,3               ;  buss mice are at a fixed address
         mov      mouse,typeB             ;
         mov      protocol,protoBS        ; Type B mouse found
         ret                              ;
cmtb2:   pop      deviceData.portAddr     ; Restore original port address
cmtb3:   stc                              ;
cmtb4:   ret                              ;

FindBussMouse endp

cseg     ends



;
; Interface:
;
;    FindBusInterrupt(busCtrl dx)
;
; Description:
;
;    Finds the interrupt for the buss mouse.  Returns the irq number in al.
; The buss mouse interrupts must be enabled on entry to this routine, they
; will be disabled on exit.
;
; Normal exit:
;
;    Carry clear -- interrupt found.
;
; Error exit:
;
;    Carry set -- interrupt not found.
;
; Side effects:
;
;    Stack is clean, registers ax, bx and cx are altered.
;

cseg     segment

         align    4

FindBusInterrupt  proc  near

         mov      cx,5                    ; Only try to get rid of multiple
fbiLoop: mov      bh,0ffh                 ;
         mov      al,irqDsbl              ; Build mask in bh to ignore irq's
         out      dx,al                   ;  which are currently changing
         MyIODelay                        ;
         call     FindBusChangingLevels   ;
         not      ah                      ;
         and      bh,ah                   ;
         mov      al,irqEnbl              ;
         out      dx,al                   ; Check for changed irq's, if
         MyIODelay                        ;  none not already found then
         call     FindBusChangingLevels   ;  go try again, or if more than
         test     al,bh                   ;  one is changing go try again
         jz       fbiAgain                ;
         call     FindBusLevel            ;
         jnc      fbiDone                 ;
fbiAgain:loop     fbiLoop                 ; Try up to 5 times
         stc                              ;
fbiDone: push     ax                      ; Disable bus irq's again and
         mov      al,irqDsbl              ;  return al set by FindBusLevel,
         out      dx,al                   ;  carry set if error
         pop      ax                      ;
         ret

FindBusInterrupt  endp

cseg     ends



;
; Interface:
;
;    FindBusChangingLevels(IrqPort dx)
;
; Description:
;
;    Finds the irq's which are changing.
;
;
; Side effects:
;
;    Stack is clean, register ax and bl are altered.
;
; Note: The active levels returned in ah are:
;
;    0000 1... Irq 2 active
;    0000 .1.. Irq 3 active
;    0000 ..1. Irq 4 active
;    0000 ...1 Irq 5 active
;

cseg     segment

         align    4

FindBusChangingLevels proc near

         push     cx                      ;
         in       al,dx                   ; Get initial irq state in bl
         MyIODelay                        ;
         mov      bl,al                   ;
         xor      ah,ah                   ;
         mov      cx,0ffffh               ;
fbcLoop: in       al,dx                   ; Get changed bits in bl, add them
         MyIODelay                        ;  to other changed bits in ah and
         xor      bl,al                   ;  put current irq state in bl
         or       ah,bl                   ;
         mov      bl,al                   ; Loop for a while to give any
         loop     fbcLoop                 ;  active irq a chance to change
         and      ah,0fh                  ;
         pop      cx                      ; Clear upper bits and return
         ret                              ;

FindBusChangingLevels endp

cseg     ends



;
; Interface:
;
;    FindBusLevel(ActiveIrqs ah)
;
; Description:
;
;    Finds the uniquely active irq.  There must be at least one level flagged
; as active.  Returns the irq number in al.
;
; Normal exit:
;
;    Carry clear -- a single level is active.
;
; Error exit:
;
;    Carry set -- multiple levels active.
;
; Side effects:
;
;    Stack is clean, registers ax is altered.
;
; Note: The active levels passed in ah are:
;
;    0000 1... Irq 2 active
;    0000 .1.. Irq 3 active
;    0000 ..1. Irq 4 active
;    0000 ...1 Irq 5 active
;

cseg     segment

         align    4

FindBusLevel proc near

         shl      ah,4                 ; Shift active levels left until we
         mov      al,1                 ;  find a one bit (there must be at
fblLoop: inc      al                   ;  least one).  Bump irq number as
         add      ah,ah                ;  we go along.
         jnc      fblLoop              ; When we find a one bit, all other
         add      ah,0ffh              ;  bits are zero if only one active
         ret                           ;  level, so set carry by adding 0ffh

FindBusLevel endp

cseg     ends



;
; Interface:
;
;    FindPs2Mouse()
;
; Description:
;
;    Checks for the existence of a properly configured PS/2 mouse.
;
; Normal exit:
;
;    Carry clear -- PS/2 mouse found.
;
; Error exit:
;
;    Carry set -- PS/2 mouse not found.
;
; Side effects:
;
;    Stack is clean, registers ax, bx, cx, dx and si are altered.
;

dseg     segment

fpmCmnd  db       0

dseg     ends

cseg     segment

         align    4

FindPs2Mouse proc near

         mov      al,mouse                ; If mouse type specified and NOT a
         cmp      al,typeS                ;  ps2 mouse then skip check, if
         jz       fpmb4                   ;  mouse specified then check anyway
         or       al,al                   ;  because we have to initialize.
         jz       fpmb0                   ;
         cmp      al,typePS               ; If a typeS mouse was specified
         jnz      fpmb5                   ;  zero mouse type.
         mov      mouse,typeErr           ;
fpmb0:   push     es                      ;
         mov      ax,biosData             ;
         mov      es,ax                   ;
         test     byte ptr es:[biosEqpt],biosMous
         pop      es                      ;
         jnz      fpmb1                   ;
         cmp      isAbios,1               ; Any machine with an ABIOS always
         jnz      fpmb5                   ;  has an auxilary mouse port
fpmb1:   call     DisableKeyboardIrq      ; Disable keyboard interrupt and
         mov      al,ps2DsKey             ;  keyboard device to prevent
         call     SendPs2Control          ;  interference during testing
         jc       fpmb3                   ;
         call     GetCommandByte          ;
         jc       fpmb3                   ; Save keyboard translation mode
         mov      fpmCmnd,al              ;
         and      al,not (ps2AuxDs+ps2KeyDs+ps2AuxEn+ps2KeyEn)
         or       al,ps2AuxEn+ps2KeyDs    ;
         call     PutCommandByte          ; Disable keyboard device and
         jc       fpmb2                   ;  enable auxilary device
         mov      al,ps2TsAux             ; Text auxilary interface, ignore
         call     SendPs2Control          ;  actual value returned because
         jc       fpmb2                   ;  some clones don't return the
         call     ReadPs2Byte             ;  correct value but DO return a
         jc       fpmb2                   ;  value
         mov      al,pixReset             ;
         call     SendPs2Command          ; Reset the mouse and then make
         jc       fpmb2                   ;  sure that it sends the power
         call     GetPs2Mouse             ;  acknowledgement followed by
         jc       fpmb2                   ;  the mouse id
         cmp      al,pixPower             ;
         jnz      fpmb2                   ;
         call     GetPs2Mouse             ;
         jc       fpmb2                   ;
         cmp      al,auxMouse             ;
         jnz      fpmb2                   ;
         mov      al,pixResln             ;
         call     SendPs2Command          ; Set up Logitech mouse to report
         jc       fpmb2                   ;  number of buttons
         mov      al,00h                  ;
         call     SendPs2Command          ;
         jc       fpmb2                   ;
         mov      al,pixScl11             ;
         call     SendPs2Command          ; Send Logitech sequence to
         jc       fpmb2                   ;  determine the number of buttons
         mov      al,pixScl11             ;
         call     SendPs2Command          ;
         jc       fpmb2                   ;
         mov      al,pixScl11             ;
         call     SendPs2Command          ;
         jc       fpmb2                   ;
         mov      al,pixStat              ;
         call     SendPs2Command          ;
         jc       fpmb2                   ;
         call     GetPs2Mouse             ;
         jc       fpmb2                   ;
         call     GetPs2Mouse             ;
         jc       fpmb2                   ; Logitech mouse returns 0x02 or
         and      ax,03h                  ;  0x03, all other mice return
         or       ax,02h                  ;  0x00 so make them 0x02 also
         mov      buttons,al              ;
         mov      bx,sync4Bit             ;
         bts      bx,ax                   ; Determine the actual sync flags
         mov      ps2Mask,bl              ;
         call     GetPs2Mouse             ;
         jc       fpmb2                   ;
         mov      al,pixRate              ;
         call     SendPs2Command          ; Set the sampling rate to 100Hz
         jc       fpmb2                   ;
         mov      al,100                  ;
         call     SendPs2Command          ;
         jc       fpmb2                   ;
         mov      al,pixResln             ;
         call     SendPs2Command          ; Set maximum resolution
         jc       fpmb2                   ;
         mov      al,3                    ;
         call     SendPs2Command          ;
         jc       fpmb2                   ;
         mov      al,pixScl11             ; Set 1:1 scaling, we handle all
         call     SendPs2Command          ;  acceleration
         jc       fpmb2                   ;
         mov      al,fpmCmnd              ;
         and      al,not (ps2AuxDs+ps2KeyDs+ps2AuxEn+ps2KeyEn)
         or       al,ps2AuxEn+ps2KeyEn    ;
         call     PutCommandByte          ;
         jc       fpmb2                   ;
         mov      deviceData.portAddr,ps2Data
         mov      deviceData.irq,ps2AxIrq ;
         mov      deviceData.mouseType,ps2Mouse
         mov      mouse,typePS            ; Type PS mouse found
         mov      protocol,protoPS        ;
         clc                              ;
         ret                              ;
fpmb2:   mov      al,fpmCmnd              ;
         and      al,not (ps2KeyDs+ps2KeyEn)
         or       al,ps2KeyEn             ;
         call     PutCommandByte          ;
fpmb3:   mov      al,ps2EnKey             ;
         call     SendPs2Control          ;
         call     EnableKeyboardIrq       ;
         stc                              ;
         ret                              ;
fpmb4:   mov      mouse,0                 ;
fpmb5:   stc                              ;
         ret                              ;

FindPs2Mouse endp

cseg     ends



;
; Interface:
;
;    FindSerialMouse()
;
; Description:
;
;    Check port address for the presence of a uart and mouse.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindSerialMouse proc near

         or       dx,dx                   ; Check for a port number
         jz       chkpBad                 ;
         push     bx                      ;
         mov      bx,offset portSave      ; Save state of serial port
         call     GetSerialState          ;
         call     CheckUartType           ; Check to see if port
         jc       chkpErr                 ;  exists, if not error
         mov      uartType,al             ;
         call     CheckSerialMouseType    ; Check for a serial mouse, if no
         jc       chkpErr                 ;  mouse then exit with error
         pop      bx                      ;
         ret                              ;
chkpErr: mov      bx,offset portSave      ;
         call     SetSerialState          ; Restore state of serial port
         pop      bx                      ;
chkpBad: mov      uartType,0              ;
         mov      mouse,0                 ;
         mov      protocol,0              ;
         stc                              ; Return error - no port
         ret                              ;

FindSerialMouse endp

cseg     ends



;
; Interface:
;
;    GetSerialState(PortState *ds:bx, portAddr dx)
;
; Description:
;
;    Saves the current state of the port whose base register address is
; passed in dx into the PortState structure whose address is passed in ds:bx.
; The contents of the receiving buffer and transmitting buffers are not
; affected, nor is the current interrupt status.  However, the complete state
; of the FIFO control register cannot be saved.  Only the trigger level can
; be saved.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

GetSerialState  proc  near

         push     dx                   ;
         add      dx,lcr               ;
         in       al,dx                ; Save contents of line control
         MyIoDelay                     ;  register, and then turn on
         mov      [bx].lcrState,al     ;  divisor latch access bit
         or       al,lcrDlab           ;
         out      dx,al                ;
         MyIoDelay                     ;
         add      dx,divMsb-lcr        ;
         in       al,dx                ; Save contents of divisor latch
         MyIoDelay                     ;
         mov      ah,al                ;
         add      dx,divLsb-divMsb     ;
         in       al,dx                ;
         MyIoDelay                     ;
         mov      [bx].divState,ax     ;
         add      dx,afr-divLsb        ;
         in       al,dx                ;
         MyIoDelay                     ;
         mov      [bx].afrState,al     ; Save contents of alternate function
         add      dx,lcr-afr           ;  register (if it exists)
         mov      al,[bx].lcrState     ;
         and      al,not lcrDlab       ; Turn off divisor latch access bit
         out      dx,al                ;
         MyIoDelay                     ;
         add      dx,mcr-lcr           ;
         in       al,dx                ; Save contents of modem control
         MyIoDelay                     ;  register
         mov      [bx].mcrState,al     ;
         add      dx,ier-mcr           ;
         in       al,dx                ; Save contents of interrupt enable
         MyIoDelay                     ;  register
         mov      [bx].ierState,al     ;
         add      dx,fcr-ier           ;
         in       al,dx                ; Save contents of FIFO control
         MyIoDelay                     ;  register -- only trigger state
         and      al,fcrTMask          ;  can be saved.
         mov      [bx].fcrState,al     ;
         add      dx,scr-fcr           ;
         in       al,dx                ; Save contents of scratch register
         MyIoDelay                     ;  (if it exists)
         mov      [bx].scrState,al     ;
         add      dx,lcr-scr           ;
         mov      al,[bx].lcrState     ; Restore divisor latch access bit
         out      dx,al                ;
         MyIoDelay                     ;
         pop      dx                   ;
         ret                           ;

GetSerialState  endp

cseg     ends



;
; Interface:
;
;    SetSerialState(PortState *ds:bx, portAddr dx)
;
; Description:
;
;    Sets the state of the port whose base register address is passed in dx
; using the PortState structure whose address is passed in ds:bx.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

SetSerialState  proc  near

         push     dx                   ;
         add      dx,lcr               ;
         mov      al,lcrDlab           ; Turn on divisor latch access bit
         out      dx,al                ;
         MyIoDelay                     ;
         add      dx,divMsb-lcr        ;
         mov      ax,[bx].divState     ;
         xchg     al,ah                ;
         out      dx,al                ; Set divisor latch
         MyIoDelay                     ;
         mov      al,ah                ;
         add      dx,divLsb-divMsb     ;
         out      dx,al                ;
         MyIoDelay                     ;
         mov      al,[bx].afrState     ;
         add      dx,afr-divLsb        ; Set alternate function register
         out      dx,al                ;  (if it exists)
         MyIoDelay                     ;
         add      dx,lcr-afr           ;
         mov      al,[bx].lcrState     ; Set line control register, but with
         and      al,not lcrDlab       ;  divisor latch access bit turned off
         out      dx,al                ;
         MyIoDelay                     ;
         add      dx,mcr-lcr           ;
         mov      al,[bx].mcrState     ;
         out      dx,al                ; Set modem control register
         MyIoDelay                     ;
         add      dx,ier-mcr           ;
         mov      al,[bx].ierState     ;
         out      dx,al                ; Set interrupt enable register
         MyIoDelay                     ;
         add      dx,fcr-ier           ;
         mov      al,[bx].fcrState     ; Set  FIFO control register
         out      dx,al                ;  (if it exists)
         MyIoDelay                     ;
         add      dx,scr-fcr           ;
         mov      al,[bx].scrState     ; Set scratch register
         out      dx,al                ;  (if it exists)
         MyIoDelay                     ;
         add      dx,lcr-scr           ;
         mov      al,[bx].lcrState     ; Set divisor latch access bit
         out      dx,al                ;
         MyIoDelay                     ;
         pop      dx                   ;
         ret                           ;

SetSerialState  endp

cseg     ends



;
; Interface:
;
;    CheckUartType(portAddr dx)
;
; Description:
;
;    Checks the type of serial port at the specified port address.
;
; Normal exit:
;
;    Carry clear.  Serial port found, register ax contains the port uart
; type...
;
;    ax = 1 INS8250, INS8250-B
;    ax = 2 INS8250A, INS82C50A, NS16450, NS16C450
;    ax = 3 NS16550
;    ax = 4 NS16550A
;    ax = 5 NS16C552
;
; Error exit:
;
;    Carry set.  No port found, ax = 0.
;
; Side effects:
;
;    Registers ax is altered.  The state of the serial port is destroyed.
;

cseg     segment

         align    4

CheckUartType proc near

         mov      al,uartType          ;
         or       al,al                ; If port type already set, then
         jnz      isPort               ;  accept it -- checking the uart
         push     dx                   ;  type glitches mice on some systems
         add      dx,lcr               ;
         mov      al,0aah              ; Test general functionality, is the
         out      dx,al                ;  core register set present?
         MyIoDelay                     ;
         in       al,dx                ; Test line control register and set
         MyIoDelay                     ;  divisor latch access bit
         cmp      al,0aah              ;
         jnz      noPort               ;
         add      dx,divMsb-lcr        ;
         mov      al,055h              ; Divisor latch should be present,
         out      dx,al                ;  check for both bytes
         MyIoDelay                     ;
         in       al,dx                ;
         MyIoDelay                     ;
         cmp      al,055h              ;
         jnz      noPort               ;
         mov      al,high div1200      ;
         out      dx,al                ;
         MyIoDelay                     ;
         add      dx,divLsb-divMsb     ;
         mov      al,0aah              ;
         out      dx,al                ;
         MyIoDelay                     ;
         in       al,dx                ;
         MyIoDelay                     ;
         cmp      al,0aah              ;
         jnz      noPort               ;
         mov      al,low div1200       ;
         out      dx,al                ;
         MyIoDelay                     ;
         mov      al,lcrBits8+lcrStop2+lcrPnone
         add      dx,lcr-divLsb        ;
         out      dx,al                ; Test line control register again
         MyIoDelay                     ;  and clear divisor access bit
         in       al,dx                ;
         MyIoDelay                     ;
         cmp      al,lcrBits8+lcrStop2+lcrPnone
         jnz      noPort               ;
         add      dx,ier-lcr           ;
         mov      al,55h               ;
         out      dx,al                ; Test interrupt enable register,
         MyIoDelay                     ;  only low 4 bits should be active
         in       al,dx                ;
         MyIoDelay                     ;
         cmp      al,05h               ;
         jnz      noPort               ;
         xor      al,al                ;
         add      dx,fcr-ier           ; Clear interrupt enable register and
         out      dx,al                ;  FIFO register (if present)
         MyIoDelay                     ;
         add      dx,ier-fcr           ;
         out      dx,al                ;
         MyIoDelay                     ;
         add      dx,iir-ier           ;
         in       al,dx                ; Interrupt identification register
         MyIoDelay                     ;  should now indicate no pending
         cmp      al,01h               ;  interrupts
         jnz      noPort               ;
         mov      al,0f4h              ;
         add      dx,mcr-iir           ; Modem control register should only
         out      dx,al                ;  have 5 bits active
         MyIoDelay                     ;
         in       al,dx                ;
         MyIoDelay                     ;
         cmp      al,014h              ;
         jnz      noPort               ;
         mov      al,10h               ;
         out      dx,al                ; Set loop mode
         MyIoDelay                     ;
         add      dx,msr-mcr           ;
         in       al,dx                ;
         MyIoDelay                     ; Test mcr/msr loopback functions
         in       al,dx                ;  - clear out delta bits
         MyIoDelay                     ;  - check state bits, should be clear
         and      al,0f0h              ;  - toggle modem control lines
         or       al,al                ;  - check state bits, should be set
         jnz      noPort               ;
         add      dx,mcr-msr           ;
         mov      al,01fh              ;
         out      dx,al                ;
         MyIoDelay                     ;
         add      dx,msr-mcr           ;
         in       al,dx                ;
         MyIoDelay                     ;
         and      al,0f0h              ;
         cmp      al,0f0h              ;
         jz       aPort                ;
noPort:  pop      dx                   ;
         xor      ax,ax                ; No port found, zero al and set carry
         stc                           ;
isPort:  ret                           ;
is8250:  pop      dx                   ;
         mov      ax,ins8250           ; 8250 found, set al and clear carry
         clc                           ;
         ret                           ;
is16450: pop      dx                   ;
         mov      ax,ns16450           ; 16450 found, set al and clear carry
         clc                           ;
         ret                           ;
is16550: pop      dx                   ;
         mov      ax,ns16550           ; 16550 found, set al and clear carry
         clc                           ;
         ret                           ;
is16550a:add      dx,lcr-afr           ;
         in       al,dx                ;
         MyIoDelay                     ;
         and      al,7fh               ; Clear divisor access bit
         out      dx,al                ;
         MyIoDelay                     ;
         pop      dx                   ;
         mov      al,ns16550a          ; 16550a found, set al and clear carry
         clc                           ;
         ret                           ;
aPort:   add      dx,mcr-msr           ;
         mov      al,mcrDtr+mcrRts     ; Exit loop mode, dtr and rts active
         out      dx,al                ;  but interrupts disabled
         MyIoDelay                     ;
         add      dx,scr-mcr           ;
         mov      al,055h              ; Check for scratch register, if not
         out      dx,al                ;  found then we have an 8250
         MyIoDelay                     ;
         in       al,dx                ;
         MyIoDelay                     ;
         cmp      al,055h              ;
         jnz      is8250               ;
         add      dx,fcr-scr           ;
         mov      al,0cfh              ; Enable FIFO buffers, check status
         out      dx,al                ;  field, if it contains
         MyIoDelay                     ;
         add      dx,iir-fcr           ;    00 - no FIFO , must be 16450
         in       al,dx                ;    10 - some FIFO, must be 16550
         MyIoDelay                     ;    11 - full FIFO, at least 16550a
         and      al,0c0h              ;
         cmp      al,080h              ;
         jz       is16550              ;
         cmp      al,0c0h              ;
         jnz      is16450              ;
         add      dx,fcr-iir           ;
         xor      al,al                ;
         out      dx,al                ; Disable FIFO
         MyIoDelay                     ;
         add      dx,lcr-fcr           ;
         in       al,dx                ;
         MyIoDelay                     ;
         or       al,80h               ; Set divisor access bit, and check
         out      dx,al                ;  for presence of alternate function
         MyIoDelay                     ;  register
         add      dx,afr-lcr           ;
         mov      al,07h               ;
         out      dx,al                ;
         MyIoDelay                     ;
         in       al,dx                ;
         MyIoDelay                     ;
         cmp      al,07h               ;
         jnz      is16550a             ;
         xor      al,al                ;
         out      dx,al                ; Clear alternate function register
         MyIoDelay                     ;
         add      dx,lcr-afr           ;
         in       al,dx                ; Clear divisor access bit
         MyIoDelay                     ;
         and      al,7fh               ;
         out      dx,al                ;
         MyIoDelay                     ;
         pop      dx                   ;
         mov      ax,ns16c552          ; 16c552 found, set al and clear carry
         clc                           ;
         ret                           ;

CheckUartType endp

cseg     ends



;
; Interface:
;
;    CheckSerialMouseType(portAddr dx)
;
; Description:
;
;    Checks the type of serial mouse at the specified port address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Register ax is altered.
;

dseg     segment

         db       0
mouseCfg db       16 dup(0)

dseg     ends

cseg     segment

         align    4

CheckSerialMouseType proc near

         push     bx                      ;
         push     cx                      ;
         push     dx                      ; Save altered registers
         push     si                      ;
         push     di                      ;
         push     es                      ;

         add      dx,mcr                  ;
         xor      al,al                   ; Set DTR and RTS low, disable
         out      dx,al                   ;  interrupts from mouse and delay
         MyIoDelay                        ;  at least 1 second to reset any
         mov      cx,1000                 ;  mouse that gets power from DTR
         call     SetupForWait            ;  and RTS
         add      dx,lsr-mcr              ;
cmt0:    sub      dx,lsr                  ;
         in       al,dx                   ; While waiting, eat outstanding
         MyIoDelay                        ;  data and errors from mouse
         add      dx,lsr                  ;
         in       al,dx                   ;
         MyIoDelay                        ;
         call     IsWaitOver              ;
         jc       cmt0                    ;

         add      dx,mcr-lsr              ;
         mov      al,mcrDtr+mcrRts        ;
         out      dx,al                   ; Set DTR and RTS high, disable
         MyIoDelay                        ;  interrupts from mouse and delay
         mov      cx,1000                 ;  at least 3/4 second to make sure
         call     SetupForWait            ;  that mouse has power
         add      dx,lsr-mcr              ;
cmt1:    sub      dx,lsr                  ;
         in       al,dx                   ; While waiting, eat outstanding
         MyIoDelay                        ;  data and errors from mouse
         add      dx,lsr                  ;
         in       al,dx                   ;
         MyIoDelay                        ;
         call     IsWaitOver              ;
         jc       cmt1                    ;

         sub      dx,lsr                  ;
         call     SerialAutoRecognize     ; Attempt to autorecognize mouse
         jc       cmt2                    ;
         call     CheckMouseTypeC         ; Check for type C mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeMorV      ; Check for type M or V mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeFX        ; Check for type FX mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeMS        ; Check for type MS mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeDW        ; Check for type DW mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeD3        ; Check for type D3 mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeD9        ; Check for type D9 mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeSX        ; Check for type SX mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeSG        ; Check for type SG mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeSM        ; Check for type SM mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeSU        ; Check for type SU mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeKR        ; Check for type KR mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeAC        ; Check for type AC mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeUN        ; Check for type UN mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeBP        ; Check for type BP mouse
         jnc      cmt2                    ;
         call     CheckMouseTypeCR        ; Check for type CR mouse
         jnc      cmt2                    ;
cmt2:    pop      es                      ;
         pop      di                      ;
         pop      si                      ; Restore registers and return
         pop      dx                      ;  mouse type
         pop      cx                      ;
         pop      bx                      ;
         ret                              ;

CheckSerialMouseType endp

cseg     ends



;
; Interface:
;
;    SerialAutoRecognize(portAddr dx)
;
; Description:
;
;    Attempts to auto-recognize a serial mouse.  Auto-recognition for serial
;    mice is performed by a central method because several of the techniques
;    can recognize multiple types of mice.  So duplicating this code for each
;    type of mouse would result in too much activity on the serial port.
;    This problem does not exist for non-serial mice so the auto-recognition
;    routines for non-serial mice are not centralized.
;
; Normal exit:
;
;    Carry clear.  Mouse was automagically recognized or set by user.  If
;    automagically recoginzed then the mouse field is set to one of...
;
;       typeC  - Logitech type C mouse
;       typeFX - Felix mouse
;       typeM  - Microsoft 2 button mouse
;       typeMS - Mouse System 3 button mouse
;       typeMW - Logitech 2 button mouse (programmable)
;       typeV  - Logitech 3 button mouse
;       typeVW - Logitech 3 button mouse (programmable)
;
; Error exit:
;
;    Carry set.  Mouse not recognized and not set by user.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;

cseg     segment

         align    4

SerialAutoRecognize proc near

         mov      al,mouse                ;
         or       al,al                   ; If mouse type already specified
         jz       sarC0                   ;  then exit
         xor      ax,ax                   ;
         ret                              ;

;
;    Check for a Logitech type C mouse by sending a protocol request and
; looking for the response.  This must be done at all baud rates because
; the mouse will only respond at its current baud rate.
;

sarC0:   mov      bx,offset port5B        ; Perform basic serial port
         call     SetSerialState          ;  initialization
         add      dx,msr                  ;
         in       al,dx                   ; If CTS is not active, then mouse
         MyIoDelay                        ;  won't accept commands, so it
         sub      dx,msr                  ;  can't be a type C mouse
         test     al,msrCts               ;
         jz       sarRTS0                 ;
         mov      ax,1200                 ;
sarC1:   push     ax                      ; Try all baud rates, mouse only
         call     SetBaudRate             ;  responds at current baud rate
         mov      al,'U'                  ;
         call     SendMouseByte           ; Put mouse into a known state,
         call     WaitForSend             ;
         mov      cx,25000                ; Type C mouse might have put a
         call     EatMouseGarbage         ;  break out, so clear error status
         mov      al,'O'                  ;  continuous incremental reporting
         call     SendMouseByte           ;  and 5B protocol
         call     WaitForSend             ;
         mov      cx,25000                ; Type C mouse might have put a
         call     EatMouseGarbage         ;  break out, so clear error status
         mov      al,'t'                  ;
         call     SendMouseByte           ; Send protocol request to mouse
         call     WaitForSend             ;
         mov      cx,250                  ; Type C mouse might have put a
         call     EatMouseGarbage         ;  break out, so clear error status
         mov      cx,100                  ;
         mov      si,2                    ; Read the response, should be
         call     ReadMouseResponse       ;  two bytes
         jc       sarC2                   ;
         cmp      mouseCfg+1,'U'          ;
         jnz      sarC2                   ; Make sure mouse is in correct
         cmp      mouseCfg+0,'O'          ;  state
         jnz      sarC2                   ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      cx,50000                ; Delay, some older Logitech mice
         call     EatMouseGarbage         ;  don't respond quickly enough
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      al,'s'                  ;
         call     SendMouseByte           ; Send status request to mouse
         call     WaitForSend             ;
         mov      cx,250                  ; Type C mouse might have put a
         call     EatMouseGarbage         ;  break out, so clear error status
         mov      cx,100                  ;
         mov      si,1                    ; Read the status byte
         call     ReadMouseResponse       ;
         jc       sarC2                   ;
         mov      cl,mouseCfg+0           ;
         and      cl,3fh                  ; Must have correct flags set
         cmp      cl,0fh                  ;
         pop      ax                      ;
         jz       sarC                    ;
         add      ax,ax                   ;
         cmp      ax,19200                ;
         jc       sarC1                   ;
         mov      al,'*'                  ;
         call     SendMouseByte           ; Change mouse to 1200 baud so
         call     WaitForSend             ;  that it will be in a standard
         mov      cx,25000                ;  state
         call     EatMouseGarbage         ;
         mov      al,'n'                  ;
         call     SendMouseByte           ;
         call     WaitForSend             ;
         mov      cx,25000                ;
         call     EatMouseGarbage         ;
         mov      ax,1200                 ;
         call     SetBaudRate             ;
         jmp      short sarRTS0           ; Type C mouse not found
sarC2:   pop      ax                      ;
         add      ax,ax                   ;
         cmp      ax,19200                ;
         jc       sarC1                   ;
         jmp      short sarRTS0           ; Type C mouse not found

;
; Toggle the RTS line and check for a response.
;

sarRTS0: mov      bx,offset portMI        ;
         call     SetSerialState          ; Perform basic serial port
         add      dx,mcr                  ;  initialization
         mov      al,mcrDtr               ;
         out      dx,al                   ; Toggle RTS low, this starts the
         MyIoDelay                        ;  reset for mice that respond to
         mov      cx,200                  ;  the RTS toggle
         call     SetupForWait            ;
         add      dx,lsr-mcr              ;
sarRTS1: sub      dx,lsr                  ;
         in       al,dx                   ; While waiting, eat outstanding
         MyIoDelay                        ;  data and errors from mouse
         add      dx,lsr                  ;
         in       al,dx                   ;
         MyIoDelay                        ;
         call     IsWaitOver              ;
         jc       sarRTS1                 ;
         add      dx,mcr-lsr              ;
         mov      al,mcrDtr+mcrRts        ; Toggle RTS high, this ends the
         out      dx,al                   ;  mouse reset
         MyIoDelay                        ;
         mov      cx,200                  ;
         call     SetupForWait            ;
         mov      byte ptr MouseCfg+0,0   ;
         mov      byte ptr MouseCfg+1,0   ; Clear response area
         mov      byte ptr MouseCfg+2,0   ;
         mov      byte ptr MouseCfg+3,0   ;
         mov      si,offset mouseCfg      ;
         sub      dx,mcr                  ; Read response from mouse, if we
sarRTS2: add      dx,lsr                  ;  have...
         in       al,dx                   ;
         MyIoDelay                        ;   B   - typeM
         sub      dx,lsr                  ;   M   - typeM or typeMW
         test     al,lsrData              ;   M3  - typeV or typeVW
         jz       sarRTS3                 ;   m   - typeMS
         in       al,dx                   ;
         MyIoDelay                        ;
         mov      byte ptr [si],al        ;
         inc      si                      ;
         cmp      si,offset mouseCfg + 3  ;
         jnc      sarRTS4                 ;
sarRTS3: call     IsWaitOver              ;
         jc       sarRTS2                 ;
sarRTS4: cmp      byte ptr mouseCfg+0,'B' ;
         jz       sarM                    ;
         cmp      byte ptr mouseCfg+0,'M' ;
         jz       sarMorV                 ;
         cmp      byte ptr mouseCfg+0,'m' ;
         jz       sarMS                   ;
         jmp      short sarBRK0           ;
sarMorV: cmp      byte ptr mouseCfg+1,'3' ;
         jnz      sarMorMW                ; Check if type M or V mouse
         call     CheckMouseTypeW         ;
         jc       sarV                    ; Check if type V or VW mouse
         jmp      sarVW                   ;
sarMorMW:call     CheckMouseTypeW         ;
         jc       sarM                    ; Check if type M or MW mouse
         jmp      sarMW                   ;

;
; Send a break and check for a response.
;

sarBRK0: mov      bx,offset portFA        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         add      dx,lcr                  ;
         in       al,dx                   ; Send a break which is a "master"
         MyIoDelay                        ;  reset
         or       al,lcrBreak             ;
         out      dx,al                   ;
         MyIoDelay                        ;
         push     ax                      ;
         sub      dx,lcr                  ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         add      dx,lcr                  ;
         pop      ax                      ;
         xor      al,lcrBreak             ;
         out      dx,al                   ;
         MyIoDelay                        ;
         sub      dx,lcr                  ;
         mov      cx,20                   ;
         mov      si,2                    ; Read response to the break,
         call     ReadMouseResponse       ;  should be #n where n is the
         jc       sarNone                 ;  version number.  Currently n
         cmp      mouseCfg[1],'#'         ;  may be 1..4
         jz       sarFX                   ;
         jmp      short sarNone           ;

;
; Mouse not automagically recognized.
;

sarNone: stc                              ; Mouse not recognized
         ret                              ;


;
; Mouse automagically recognized.
;

sarC:    mov      buttons,3               ;
         mov      mouse,typeC             ; Logitech type C mouse found
         mov      autoMag,1               ;
         or       mseSpeed,mse1200+mse2400+mse4800+mse9600
         ret                              ;

sarFX:   mov      mouse,typeFX            ;
         mov      buttons,3               ; Felix mouse
         mov      autoMag,1               ;
         or       mseSpeed,mse9600        ;
         ret                              ;

sarM:    mov      mouse,typeM             ;
         mov      buttons,2               ; Microsoft 2 button mouse
         mov      autoMag,1               ;
         or       mseSpeed,mse1200        ;
         ret                              ;

sarMS:   mov      mouse,typeMS            ;
         mov      buttons,3               ; Mouse System 3 button mouse
         mov      autoMag,1               ;
         or       mseSpeed,mse1200        ;
         ret                              ;

sarMW:   mov      mouse,typeMW            ;
         mov      autoMag,1               ;
         or       mseSpeed,mse1200        ; Logitech 2 button programmable
         ret                              ;  mouse

sarV:    mov      mouse,typeV             ;
         mov      buttons,3               ; Logitech 3 button mouse
         mov      autoMag,1               ;
         or       mseSpeed,mse1200+mse9600;
         ret                              ;

sarVW:   mov      mouse,typeVW            ;
         mov      autoMag,1               ; Logitech 3 button programmable
         or       mseSpeed,mse1200        ;  mouse
         ret                              ;

SerialAutoRecognize endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeW(mouseType al, portAddr dx)
;
; Description:
;
;    Assuming that a type M or V mouse has been found, this routine checks
; for a type W mouse.
;
; Normal exit:
;
;    Carry clear.  Type W mouse found.
;
; Error exit:
;
;    Carry set.  Type W mouse not found.
;
; Side effects:
;
;    Registers ax, bx, cx, si, di and es are altered.
;

cseg     segment

         align    4

CheckMouseTypeW proc near

         add      dx,msr                  ;
         in       al,dx                   ; If CTS is not active, then mouse
         MyIoDelay                        ;  won't accept commands, so it
         sub      dx,msr                  ;  can't be a type W mouse
         test     al,msrCts               ;
         jz       cmtw3                   ; All commands must be transmitted
         mov      ax,1200                 ;  at 1200 baud for type W mice
         call     SetBaudRate             ;
         mov      al,'?'                  ; Send *? command to get type W
         call     SendMouseCommand        ;  configuration information
         mov      si,4                    ;
         mov      cx,200                  ;
         call     ReadMouseResponse       ; Read up to four bytes from mouse,
         jc       cmtw4                   ;  the first byte must have the
         test     mouseCfg+3,sync7Bit     ;  sync bit set
         jz       cmtw3                   ;
         mov      al,mouseCfg+2           ;
         shr      al,3                    ; Save extended command capability
         and      al,01h                  ;  used with radio mouse for
         mov      statsChk,al             ;  battery status
         mov      al,mouseCfg+2           ;
         shl      al,2                    ; Save 9600 baud capability
         and      al,mse9600              ;
         or       mseSpeed,al             ;
         mov      al,mouseCfg+1           ;
         shr      al,3                    ; Save number of buttons
         and      al,07h                  ;
         mov      buttons,al              ;
         cmp      statsChk,0              ;
         jz       cmtw2                   ;
         mov      al,'!'                  ; Send *! command to get type W
         call     SendMouseCommand        ;  configuration information
         mov      si,7                    ;
         mov      cx,300                  ;
         call     ReadMouseResponse       ; Read up to seven bytes from mouse,
         jc       cmtw1                   ;  the first byte must have the
         test     mouseCfg+6,sync7Bit     ;  sync bit set
         jz       cmtw2                   ;
         mov      al,mouseCfg+6           ;
         and      al,3fh                  ; If not a radio mouse then we
         cmp      al,1                    ;  are done
         jnz      cmtw1                   ;
         mov      al,mouseCfg+5           ;
         and      al,03h                  ; If battery is low then set the
         cmp      al,2                    ;  battery low flag
         jnz      cmtw1                   ;
         mov      battLow,1               ;
cmtw1:   cmc                              ; Type W mouse found
cmtw2:   ret                              ;
cmtw3:   stc                              ; Type W mouse not found
cmtw4:   ret                              ;

CheckMouseTypeW endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeC(mouseType ax, portAddr dx)
;
; Description:
;
;    Checks for type C mouse at the specified port address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, si, di and es are altered.
;

cseg     segment

         align    4

CheckMouseTypeC proc near

         mov      al,mouse                ; Check for mouse type C.  The mouse
         cmp      al,typeC                ;  must have been automagically
         jnz      cmtc2                   ;  recognized or set by the user
         cmp      autoMag,0               ;
         jnz      cmtc0                   ;

         add      dx,msr                  ;
         in       al,dx                   ; If CTS is not active, then mouse
         MyIoDelay                        ;  won't accept commands, so it
         sub      dx,msr                  ;  can't be a type C mouse
         test     al,msrCts               ;
         jz       cmtc2                   ;
         mov      bx,offset port5B        ; Perform basic serial port
         call     SetSerialState          ;  initialization

         mov      ax,1200                 ;
         call     SetBaudRate             ; State of mouse is not known, so
         mov      al,'*'                  ;  attempt to force the mouse to
         call     SendMouseByte           ;  1200 baud -- this approach
         call     WaitForSend             ;  differs from autorecognition in
         mov      cx,25000                ;  that no checking is done so it
         call     EatMouseGarbage         ;  may work on troublesome mice
         mov      al,'q'                  ;
         call     SendMouseByte           ;
         call     WaitForSend             ;
         mov      cx,25000                ;
         call     EatMouseGarbage         ;

         mov      ax,9600                 ;
         call     SetBaudRate             ;
         mov      al,'*'                  ;
         call     SendMouseByte           ;
         call     WaitForSend             ;
         mov      cx,25000                ;
         call     EatMouseGarbage         ;
         mov      al,'p'                  ;
         call     SendMouseByte           ;
         call     WaitForSend             ;
         mov      cx,25000                ;
         call     EatMouseGarbage         ;

         mov      ax,4800                 ;
         call     SetBaudRate             ;
         mov      al,'*'                  ;
         call     SendMouseByte           ;
         call     WaitForSend             ;
         mov      cx,25000                ;
         call     EatMouseGarbage         ;
         mov      al,'o'                  ;
         call     SendMouseByte           ;
         call     WaitForSend             ;
         mov      cx,25000                ;
         call     EatMouseGarbage         ;

         mov      ax,2400                 ;
         call     SetBaudRate             ;
         mov      al,'*'                  ;
         call     SendMouseByte           ;
         call     WaitForSend             ;
         mov      cx,25000                ;
         call     EatMouseGarbage         ;
         mov      al,'n'                  ;
         call     SendMouseByte           ;
         call     WaitForSend             ;
         mov      cx,25000                ;
         call     EatMouseGarbage         ;

         or       mseSpeed,mse1200+mse2400+mse4800+mse9600
         mov      buttons,3               ;

cmtc0:   mov      al,'U'                  ;
         call     SendMouseByte           ; Put mouse into a known state,
         call     WaitForSend             ;  continuous incremental reporting
         mov      cx,25000                ;  and 5B protocol
         call     EatMouseGarbage         ;
         mov      al,'O'                  ;
         call     SendMouseByte           ;
         call     WaitForSend             ;
         mov      cx,25000                ;
         call     EatMouseGarbage         ;

         mov      al,protocol             ;
         cmp      al,protoMM              ;
         jz       cmtc1                   ; Check protocol, type C mice
         cmp      al,protoRE              ;  understand the 5B, MM and RE
         jz       cmtc1                   ;  protocols.  If no protocol or
         cmp      al,proto5B              ;  illegal protocol then default
         jz       cmtc1                   ;  to 5B (normal reset default)
         mov      protocol,proto5B        ;
         clc                              ;
cmtc1:   ret                              ;
cmtc2:   stc                              ; Type C mouse not found
         ret                              ;

CheckMouseTypeC endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeMorV(portAddr dx)
;
; Description:
;
;    Checks for type M or V mouse at the specified port address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;

cseg     segment

         align    4

CheckMouseTypeMorV proc near

         mov      al,mouse                ;
         cmp      al,typeM                ; Check for mouse type M, MW, V
         jz       cmtIsM                  ;  or VW.  The mouse type must
         cmp      al,typeMW               ;  have been automagically
         jz       cmtIsMW                 ;  recognized or set by the user.
         cmp      al,typeV                ;
         jz       cmtIsV                  ;
         cmp      al,typeVW               ;
         jz       cmtIsVW                 ;
         stc                              ;
         ret                              ;

cmtIsM:  cmp      autoMag,0               ;
         jnz      cmtIsM0                 ;
         mov      bx,offset portMI        ; Perform basic serial port
         call     SetSerialState          ;  initialization
         or       mseSpeed,mse1200        ;
         mov      buttons,2               ; Type M mice understand the MI
cmtIsM0: mov      al,protocol             ;  protocol
         cmp      al,protoMI              ;
         jz       cmtIsM1                 ;
         mov      protocol,protoMI        ;
         clc                              ;
cmtIsM1: ret                              ;

cmtIsMW: cmp      autoMag,0               ;
         jnz      cmtIsMW0                ; Perform basic serial port
         mov      bx,offset portMI        ;  initialization
         call     SetSerialState          ;
         or       mseSpeed,mse1200+mse9600;
         mov      buttons,2               ; Check protocol, type MW mice
cmtIsMW0:mov      al,protocol             ;  mice understand the MI, MP
         cmp      al,proto5B              ;  and 5B protocols.
         jz       cmtIsMW1                ;
         cmp      al,protoMI              ; If no protocol or illegal
         jz       cmtIsMW1                ;  protocol then default to MI
         cmp      al,protoMP              ;  (normal reset default)
         jz       cmtIsMW1                ;
         mov      protocol,protoMI        ;
         clc                              ;
cmtIsMW1:ret                              ;

cmtIsV:  cmp      autoMag,0               ;
         jnz      cmtIsV0                 ; Perform basic serial port
         mov      bx,offset portMP        ;  initialization
         call     SetSerialState          ;
         or       mseSpeed,mse1200        ;
         mov      buttons,3               ; Check protocol, type V mice
cmtIsV0: mov      al,protocol             ;  understand the MI and MP
         cmp      al,protoMI              ;  protocols
         jz       cmtIsV1                 ;
         cmp      al,protoMP              ; If no protocol or illegal
         jz       cmtIsV1                 ;  protocol then default to MP
         mov      protocol,protoMP        ;  (normal reset default)
         clc                              ;
cmtIsV1: ret                              ;

cmtIsVW: cmp      autoMag,0               ;
         jnz      cmtIsVW0                ; Perform basic serial port
         mov      bx,offset portMP        ;  initialization
         call     SetSerialState          ;
         or       mseSpeed,mse1200+mse9600;
         mov      buttons,3               ; Check protocol, type VW mice
cmtIsVW0:mov      al,protocol             ;  mice understand the MI, MP
         cmp      al,proto5B              ;  and 5B protocols.
         jz       cmtIsVW1                ;
         cmp      al,protoMI              ; If no protocol or illegal
         jz       cmtIsVW1                ;  protocol then default to MP
         cmp      al,protoMP              ;  (normal reset default)
         jz       cmtIsVW1                ;
         mov      protocol,protoMP        ;
         clc                              ;
cmtIsVW1:ret                              ;

CheckMouseTypeMorV endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeFX(portAddr dx)
;
; Description:
;
;    Checks for a Felix mouse at the specified port address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;
; Note: The Felix mouse has inconsistent command sets for programming the
;       baud rate, so only the default baud rate of 9600 baud is used.  Since
;       the mouse only sends data when in the margins or when the data has
;       changed this should not burden the system.  Also the CTS line is not
;       used.
;

cseg     segment

CheckMouseTypeFX proc near

         mov      al,mouse                ;
         cmp      al,typeFX               ; Check if mouse type is already
         jnz      cmtfx1                  ;  set by user -- if so then it
         mov      mouse,typeErr           ;  must be type FX.
         cmp      autoMag,0               ;
         jnz      cmtfx0                  ;
         mov      bx,offset portFA        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         add      dx,lcr                  ;
         in       al,dx                   ; Send a break which is a "master"
         MyIoDelay                        ;  reset, on newer Felix mice this
         or       al,lcrBreak             ;  also sets the mouse into
         out      dx,al                   ;  absolute mode
         MyIoDelay                        ;
         push     ax                      ;
         sub      dx,lcr                  ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         add      dx,lcr                  ;
         pop      ax                      ;
         xor      al,lcrBreak             ;
         out      dx,al                   ;
         MyIoDelay                        ;
         sub      dx,lcr                  ;
         mov      cx,100                  ;
         mov      si,2                    ; Read response to the break,
         call     ReadMouseResponse       ;  should be #n where n is the
         jc       cmtfx2                  ;  version number
         cmp      mouseCfg[1],'#'         ;
         jnz      cmtfx1                  ;
         cmp      mouseCfg[0],'0'         ;
         jb       cmtfx2                  ;
         cmp      mouseCfg[0],'9'         ;
         ja       cmtfx2                  ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
cmtfx0:  mov      al,'s'                  ;
         call     SendMouseByte           ; Set sample rate to 100us
         mov      al,100                  ;
         call     SendMouseByte           ;
         xor      al,al                   ;
         call     SendMouseByte           ;
         call     WaitForSend             ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      al,'p'                  ;
         call     SendMouseByte           ; Set packet rate to 100Hz
         mov      al,100                  ;
         call     SendMouseByte           ;
         xor      al,al                   ;
         call     SendMouseByte           ;
         call     WaitForSend             ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      buttons,3               ;
         or       mseSpeed,mse9600        ;
         mov      mouse,typeFX            ; Type FX mouse found
         cmp      protocol,protoFR        ;  only FA and FR protocols
         jz       cmtfx2                  ;  permitted
         mov      protocol,protoFA        ;
         clc                              ;
         ret                              ;
cmtfx1:  stc                              ; Type FX mouse not found
cmtfx2:  ret                              ;

CheckMouseTypeFX endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeMS(portAddr dx)
;
; Description:
;
;    Checks for Mouse System's mouse at the specified port address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;

cseg     segment

CheckMouseTypeMS proc near

         cmp      mouse,typeMS            ;
         jnz      cmtms1                  ; MOUSE=MS is required to recognize
         cmp      autoMag,0               ;  a Mouse System's mouse
         jnz      cmtms0                  ;
         mov      bx,offset port5B        ;
         call     SetSerialState          ; Setup serial port initialization
         or       mseSpeed,mse1200        ;
         mov      buttons,3               ; Type MS mouse found
cmtms0:  mov      protocol,proto5B        ;  only 5B protocol permitted
         clc                              ;
         ret                              ;
cmtms1:  stc                              ; Type MS mouse not found
         ret                              ;

CheckMouseTypeMS endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeDW(portAddr dx)
;
; Description:
;
;    Checks for CalComp's Wiz/2x00 digitizer at the specified port address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;
; Note: The digitizer is set to 1000 lpi, this prevents even the largest
;       digitizer tablets (44" x 60") from exceeding the OS/2 limits.  Even
;       on a small digitizer tablet (7.5" x 7.5") this gives more than enough
;       resolution for the largest possible screens.
;

dseg     segment

digWiz   db       '2G2G2GjS',0
digRate  db       escape,'%C1N81',cr,0
digPkt23 db       escape,'%^23',cr,0
digLpi   db       escape,'%JR1000,0',cr,0
digIncX  db       escape,'%X1',cr,0
digIncY  db       escape,'%Y1',cr,0
digRep   db       escape,'%W125',cr,0
digHalt  db       escape,'%H',cr,0
digSize  db       escape,'%VS',cr,0
digIncRn db       escape,'%IR',cr,0
digMrgns db       escape,'%Z0',cr,0

dseg     ends

cseg     segment

CheckMouseTypeDW proc near

         cmp      mouse,typeDW            ; MOUSE=DW is required to recognize
         jnz      cmtd6                   ;  CalComp WIZ/2x00 digitizer
         mov      mouse,typeErr           ;
         add      dx,msr                  ;
         in       al,dx                   ; If CTS is not active, then mouse
         MyIoDelay                        ;  won't accept commands, so it
         sub      dx,msr                  ;  can't be a type DW mouse
         test     al,msrCts               ;
         jz       cmtd6                   ;
         mov      bx,offset portDG        ; Perform setup serial port
         call     SetSerialState          ;  initialization and reset
         mov      bx,offset digWiz        ;  tablet
         call     SendMouseString         ;
         mov      bx,offset digRate       ; Set 9600 baud, no parity, 8 data
         call     SendMouseString         ;  bits and one stop bit
         mov      bx,offset portHA        ; Perform basic serial port
         call     SetSerialState          ;
         mov      bx,offset digPkt23      ;
         call     SendMouseString         ; Set CalComp High Resolution mode
         mov      bx,offset digLpi        ;
         call     SendMouseString         ; Set 1000 lpi
         mov      bx,offset digIncX       ;
         call     SendMouseString         ; Set X and Y increments
         mov      bx,offset digIncY       ;
         call     SendMouseString         ;
         mov      bx,offset digRep        ; Set reporting rate to 125
         call     SendMouseString         ;
         mov      bx,offset digHalt       ; Stop all data reporting
         call     SendMouseString         ;
         mov      cx,25000                ; Eat any outstanding data packets
         call     EatMouseGarbage         ;
         mov      bx,offset digSize       ; Ask tablet for its size
         call     SendMouseString         ;
         mov      cx,100                  ;
         mov      si,6                    ; Read the response which will be
         call     ReadMouseResponse       ;  formatted like a data packet
         jc       cmtd7                   ;
         push     dx                      ;
         mov      dh,mouseCfg[5]          ;
         mov      dl,mouseCfg[4]          ; Put X maximum position in dx
         add      dl,dl                   ;
         shl      dx,6                    ;
         or       dl,mouseCfg[3]          ; Put Y maximum position in bx
         mov      bh,mouseCfg[2]          ;
         mov      bl,mouseCfg[1]          ;
         add      bl,bl                   ;
         shl      bx,6                    ;
         or       bl,mouseCfg[0]          ;
         cmp      bx,1000                 ; Make sure table is at least one
         jc       cmtd5                   ;  inch square
         cmp      dx,1000                 ;
         jc       cmtd5                   ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtd1                   ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtd2                   ;
cmtd1:   mov      rowMax,bx               ;
cmtd2:   cmp      colMax,0                ;
         jz       cmtd3                   ;
         cmp      colMax,dx               ;
         jb       cmtd4                   ;
cmtd3:   mov      colMax,dx               ;
cmtd4:   pop      dx                      ;
         mov      bx,offset digIncRn      ; Set incremental run mode
         call     SendMouseString         ;
         mov      bx,offset digMrgns      ; Allow reporting in tablet margin
         call     SendMouseString         ;  to prevent "cliff" effect
         or       mseSpeed,mse9600        ;
         mov      mouse,typeDW            ; Type DW mouse found
         mov      buttons,3               ;  only HA and HR protocols
         cmp      protocol,protoHR        ;  permitted
         jz       cmtd7                   ;
         mov      protocol,protoHA        ;
         clc                              ;
         ret                              ;
cmtd5:   pop      dx                      ;
cmtd6:   stc                              ; Type DW mouse not found
cmtd7:   ret                              ;

CheckMouseTypeDW endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeD3(portAddr dx)
;
; Description:
;
;    Checks for CalComp's 3300 or newer digitizer at the specified port
; address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;
; Note: The digitizer is set to 1000 lpi, this prevents even the largest
;       digitizer tablets (44" x 60") from exceeding the OS/2 limits.  Even
;       on a small digitizer tablet (7.5" x 7.5") this gives more than enough
;       resolution for the largest possible screens.
;

dseg     segment

dig3300  db       escape,'%VR5',cr,0
digStart db       escape,'%A1',cr,0
digPrsr  db       escape,'%VA0',cr,0
digThshd db       escape,'%VAV4',cr,0,0,0
digProx  db       escape,'%Z3',cr,0

dseg     ends

cseg     segment

CheckMouseTypeD3 proc near

         cmp      mouse,typeD3            ; MOUSE=D3 is required to recognize
         jnz      cmtd3n                  ;  CalComp 3300 digitizer
         mov      mouse,typeErr           ;
         add      dx,msr                  ;
         in       al,dx                   ; If CTS is not active, then mouse
         sub      dx,msr                  ;  won't accept commands, so it
         test     al,msrCts               ;  can't be a type D3 mouse
         jz       cmtd3n                  ;
         mov      bx,offset portDG        ; Perform setup serial port
         call     SetSerialState          ;  initialization and reset
         mov      cx,1                    ;
         call     SetupForWait            ; Wait for timer tick edge and then
cmtd3a:  call     IsWaitOver              ;  toggle DTS low and then high 6
         jc       cmtd3a                  ;  times.  The width of each pulse
         add      dx,mcr                  ;  must be between 50 and 100 ms.
         mov      cx,6                    ;
cmtd3b:  push     cx                      ; Since we waited for the edge of
         mov      al,0                    ;  a timer tick, the timer between
         out      dx,al                   ;  edges will be the smallest
         mov      cx,50                   ;  multiple of the timer tick, but
         call     SetupForWait            ;  at least 50 ms.  Since OS/2 has
cmtd3c:  call     IsWaitOver              ;  a timer tick no wider than 32 ms
         jc       cmtd3c                  ;  this satisfies the timing
         mov      al,mcrDtr+mcrRts        ;  constraints.
         out      dx,al                   ;
         mov      cx,50                   ; This "wiggle" will reset the 3300
         call     SetupForWait            ;  and newer tablets
cmtd3d:  call     IsWaitOver              ;
         jc       cmtd3d                  ;
         pop      cx                      ;
         dec      cx                      ;
         jnz      cmtd3b                  ;
         sub      dx,mcr                  ;
         mov      bx,offset portHA        ; Perform basic serial port
         call     SetSerialState          ;  initialization
         mov      cx,25000                ; Eat any outstanding data packets
         call     EatMouseGarbage         ;
         mov      bx,offset dig3300       ; Initialize tablet
         call     SendMouseString         ;
         mov      bx,offset digLpi        ; Set 1000 lpi
         call     SendMouseString         ;
         mov      bx,offset digSize       ; Ask tablet for its size
         call     SendMouseString         ;
         mov      cx,100                  ;
         mov      si,6                    ; Read the response which will be
         call     ReadMouseResponse       ;  formatted like a data packet
         jc       cmtd3n                  ;
         push     dx                      ;
         mov      dh,mouseCfg[5]          ;
         mov      dl,mouseCfg[4]          ; Put X maximum position in dx
         add      dl,dl                   ;
         shl      dx,6                    ;
         or       dl,mouseCfg[3]          ; Put Y maximum position in bx
         mov      bh,mouseCfg[2]          ;
         mov      bl,mouseCfg[1]          ;
         add      bl,bl                   ;
         shl      bx,6                    ;
         or       bl,mouseCfg[0]          ;
         cmp      bx,1000                 ; Make sure table is at least one
         jc       cmtd3m                  ;  inch square
         cmp      dx,1000                 ;
         jc       cmtd3m                  ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtd3e                  ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtd3f                  ;
cmtd3e:  mov      rowMax,bx               ;
cmtd3f:  cmp      colMax,0                ;
         jz       cmtd3g                  ;
         cmp      colMax,dx               ;
         jb       cmtd3h                  ;
cmtd3g:  mov      colMax,dx               ;
cmtd3h:  pop      dx                      ;
         mov      bx,offset digPrsr       ; Disable pressure data
         call     SendMouseString         ;
         mov      ax,pressure             ;
         cmp      ax,0ffffh               ;
         je       cmtd3l                  ;
         cmp      ax,1                    ; Must be in the range 1..255 for
         jb       cmtd3o                  ;  CalComp digitizer
         cmp      ax,255                  ;
         ja       cmtd3n                  ;
         mov      si,offset digThshd+5    ;
         mov      bl,100                  ;
         div      bl                      ; Build pressure command
         mov      bl,10                   ;
         or       al,al                   ;
         jnz      cmtd3i                  ;
         movzx    ax,ah                   ;
         div      bl                      ;
         or       al,al                   ;
         jnz      cmtd3j                  ;
         jmp      short cmtd3k            ;
cmtd3i:  add      al,'0'                  ;
         mov      byte ptr [si],al        ;
         inc      si                      ;
         movzx    ax,ah                   ;
         div      bl                      ;
cmtd3j:  add      al,'0'                  ;
         mov      byte ptr [si],al        ;
         inc      si                      ;
cmtd3k:  add      ah,'0'                  ;
         mov      byte ptr [si],ah        ;
         inc      si                      ;
         mov      byte ptr [si],cr        ;
cmtd3l:  mov      bx,offset digThshd      ; Set pressure pen threshold
         call     SendMouseString         ;
         mov      bx,offset digProx       ; Set proximity to low
         call     SendMouseString         ;
         mov      bx,offset digStart      ; Start tablet sending data
         call     SendMouseString         ;
         or       mseSpeed,mse150+mse300+mse600+mse1200+mse2400+mse4800+mse9600+mse19200
         mov      buttons,3               ;
         mov      mouse,typeD3            ; Type D3 mouse found
         cmp      protocol,protoHR        ;  only HA and HR protocols
         jz       cmtd3o                  ;  permitted
         mov      protocol,protoHA        ;
         clc                              ;
         ret                              ;
cmtd3m:  pop      dx                      ;
cmtd3n:  stc                              ; Type D3 mouse not found
cmtd3o:  ret                              ;

CheckMouseTypeD3 endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeD9(portAddr dx)
;
; Description:
;
;    Checks for CalComp's 9x00 digitizer at the specified port address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;
; Note: The digitizer is set to 1000 lpi, this prevents even the largest
;       digitizer tablets (44" x 60") from exceeding the OS/2 limits.  Even
;       on a small digitizer tablet (7.5" x 7.5") this gives more than enough
;       resolution for the largest possible screens.
;

dseg     segment

digBeep  db       escape,'%V8',cr,0

dseg     ends

cseg     segment

CheckMouseTypeD9 proc near

         cmp      mouse,typeD9            ; MOUSE=D9 is required to recognize
         jnz      cmtd9f                  ;  CalComp 9x00 digitizer
         mov      mouse,typeErr           ;
         add      dx,msr                  ;
         in       al,dx                   ; If CTS is not active, then mouse
         MyIoDelay                        ;  won't accept commands, so it
         sub      dx,msr                  ;  can't be a type D9 mouse
         test     al,msrCts               ;
         jz       cmtd9f                  ;
         mov      bx,offset portDG        ; Perform setup serial port
         call     SetSerialState          ;  initialization and reset
         mov      bx,offset digBeep       ;  tablet
         call     SendMouseString         ;
         mov      bx,offset digRate       ; Set 9600 baud, no parity, 8 data
         call     SendMouseString         ;  bits and one stop bit
         mov      bx,offset portHA        ; Perform basic serial port
         call     SetSerialState          ;
         mov      bx,offset digPkt23      ;
         call     SendMouseString         ; Set CalComp High Resolution mode
         mov      bx,offset digLpi        ;
         call     SendMouseString         ; Set 1000 lpi
         mov      bx,offset digIncX       ;
         call     SendMouseString         ; Set X and Y increments
         mov      bx,offset digIncY       ;
         call     SendMouseString         ;
         mov      bx,offset digRep        ; Set reporting rate to 125
         call     SendMouseString         ;
         mov      bx,offset digHalt       ; Stop all data reporting
         call     SendMouseString         ;
         mov      cx,25000                ; Eat any outstanding data packets
         call     EatMouseGarbage         ;
         mov      bx,offset digSize       ; Ask tablet for its size
         call     SendMouseString         ;
         mov      cx,100                  ;
         mov      si,6                    ; Read the response which will be
         call     ReadMouseResponse       ;  formatted like a data packet
         jc       cmtd9f                  ;
         push     dx                      ;
         mov      dh,mouseCfg[5]          ;
         mov      dl,mouseCfg[4]          ; Put X maximum position in dx
         add      dl,dl                   ;
         shl      dx,6                    ;
         or       dl,mouseCfg[3]          ; Put Y maximum position in bx
         mov      bh,mouseCfg[2]          ;
         mov      bl,mouseCfg[1]          ;
         add      bl,bl                   ;
         shl      bx,6                    ;
         or       bl,mouseCfg[0]          ;
         cmp      bx,1000                 ; Make sure table is at least one
         jc       cmtd9e                  ;  inch square
         cmp      dx,1000                 ;
         jc       cmtd9e                  ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtd9a                  ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtd9b                  ;
cmtd9a:  mov      rowMax,bx               ;
cmtd9b:  cmp      colMax,0                ;
         jz       cmtd9c                  ;
         cmp      colMax,dx               ;
         jb       cmtd9d                  ;
cmtd9c:  mov      colMax,dx               ;
cmtd9d:  pop      dx                      ;
         mov      bx,offset digIncRn      ; Set incremental run mode
         call     SendMouseString         ;
         mov      bx,offset digMrgns      ; Allow reporting in tablet margin
         call     SendMouseString         ;  to prevent "cliff" effect
         or       mseSpeed,mse9600        ;
         mov      buttons,3               ;
         mov      mouse,typeD9            ; Type D9 mouse found
         cmp      protocol,protoHR        ;  only HA and HR protocols
         jz       cmtd9g                  ;  permitted
         mov      protocol,protoHA        ;
         clc                              ;
         ret                              ;
cmtd9e:  pop      dx                      ;
cmtd9f:  stc                              ; Type D9 mouse not found
cmtd9g:  ret                              ;

CheckMouseTypeD9 endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeSG(portAddr dx)
;
; Description:
;
;    Checks for SummaGraphic's digitizer at the specified port address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;
; Note: Summagraphics digitizers default to 500 dpi in MM mode and 1000 dpi
;       when in UIOF mode.
;

dseg     segment

sgXon    db       xon,0
sgSetup  db       'z9zb',0
sgPuck   db       'zt',0
sgSize   db       'a',0
sgUiof   db       'zu',0
suReset  db       escape,'Z',0
suSetup  db       escape,'B1',escape,'p2',0
suSize   db       escape,'a',0

dseg     ends

cseg     segment

CheckMouseTypeSG proc near

         cmp      mouse,typeSG            ; MOUSE=SG is required to recognize
         jnz      cmtsg7                  ;  a SummaGraphic's digitizer
         mov      mouse,typeErr           ;
         add      dx,msr                  ;
         in       al,dx                   ; If CTS is not active, then mouse
         MyIoDelay                        ;  won't accept commands, so it
         sub      dx,msr                  ;  can't be a type SG mouse
         test     al,msrCts               ;
         jz       cmtsg7                  ;
         mov      bx,offset portMA        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         add      dx,lcr                  ;
         in       al,dx                   ; Send a break which is a "master"
         MyIoDelay                        ;  reset.  This forces the tablet
         or       al,lcrBreak             ;  into MM mode regardless of its
         out      dx,al                   ;  current state and of the
         MyIoDelay                        ;  attached cursor
         push     ax                      ;
         sub      dx,lcr                  ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         add      dx,lcr                  ;
         pop      ax                      ;
         xor      al,lcrBreak             ;
         out      dx,al                   ;
         MyIoDelay                        ;
         sub      dx,lcr                  ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset sgPuck        ;
         call     SendMouseQuery          ; Request puck type, the response
         mov      cx,250                  ;  will be "CSR4", "CSR16" or
         mov      si,9                    ;  "STYLUS" followed by a <cr>
         call     ReadMouseResponse       ;
         mov      buttons,3               ;
         cmp      si,2                    ; If a STYLUS then we only have
         jnz      cmtsg1                  ;  two buttons, otherwise three
         mov      buttons,2               ;
cmtsg1:  cmp      protocol,protoUA        ; If UIOF protocol to be used then
         jz       cmtsgu1                 ;  setup is completely different
         cmp      protocol,protoUR        ;
         jz       cmtsgu1                 ;
         mov      bx,offset sgSetup       ; Make sure 8 data, 1 stop, odd
         call     SendMouseString         ;  parity, eat returned ack
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset sgSize        ;
         call     SendMouseQuery          ;
         mov      cx,250                  ;
         mov      si,5                    ;
         call     ReadMouseResponse       ;
         jc       cmtsg8                  ;
         push     dx                      ;
         xor      dx,dx                   ;
         mov      dh,mouseCfg[2]          ; Put X maximum position in dx
         shr      dx,1                    ;
         or       dl,mouseCfg[3]          ;
         xor      bx,bx                   ;
         mov      bh,mouseCfg[0]          ; Put Y maximum position in bx
         shr      bx,1                    ;
         mov      bl,mouseCfg[1]          ;
         cmp      bx,500                  ; Make sure table is at least one
         jc       cmtsg6                  ;  inch square
         cmp      dx,500                  ;
         jc       cmtsg6                  ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtsg2                  ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtsg3                  ;
cmtsg2:  mov      rowMax,bx               ;
cmtsg3:  cmp      colMax,0                ;
         jz       cmtsg4                  ;
         cmp      colMax,dx               ;
         jb       cmtsg5                  ;
cmtsg4:  mov      colMax,dx               ;
cmtsg5:  pop      dx                      ;
         or       mseSpeed,mse9600        ;
         mov      mouse,typeSG            ; Type SG mouse found
         cmp      protocol,protoMR        ;  only MM, MA and MR protocols
         jz       cmtsg8                  ;  permitted
         cmp      protocol,protoMM        ;
         jz       cmtsg8                  ;
         mov      protocol,protoMA        ;
         clc                              ;
         ret                              ;
cmtsg6:  pop      dx                      ;
cmtsg7:  stc                              ; Type SG mouse not found
cmtsg8:  ret                              ;
cmtsgu1: mov      bx,offset sgUiof        ;
         call     SendMouseString         ; Enable UIOF protocol
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset portUA        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset suSetup       ; Make sure 7 data, 2 stop, even
         call     SendMouseString         ;  parity, eat returned ack
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset sgXon         ; Send x-on to enable transmissions
         call     SendMouseString         ;  and eat any outstanding data
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset suSize        ;
         call     SendMouseQuery          ;
         mov      cx,250                  ;
         mov      si,8                    ;
         call     ReadMouseResponse       ;
         jc       cmtsgu7                 ;
         push     dx                      ;
         mov      dl,mouseCfg[4]          ; Put X maximum position in dx
         mov      dh,mouseCfg[3]          ;
         shl      dl,2                    ;
         shl      dx,4                    ;
         or       dl,mouseCfg[5]          ;
         mov      bl,mouseCfg[1]          ;
         mov      bh,mouseCfg[0]          ; Put Y maximum position in bx
         shl      bl,2                    ;
         shl      bx,4                    ;
         or       bl,mouseCfg[2]          ;
         cmp      bx,1000                 ; Make sure table is at least one
         jc       cmtsgu6                 ;  inch square
         cmp      dx,1000                 ;
         jc       cmtsgu6                 ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtsgu2                 ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtsgu3                 ;
cmtsgu2: mov      rowMax,bx               ;
cmtsgu3: cmp      colMax,0                ;
         jz       cmtsgu4                 ;
         cmp      colMax,dx               ;
         jb       cmtsgu5                 ;
cmtsgu4: mov      colMax,dx               ;
cmtsgu5: pop      dx                      ;
         or       mseSpeed,mse9600        ;
         mov      mouse,typeSG            ; Type SG mouse found
         ret                              ;
cmtsgu6: pop      dx                      ;
         stc                              ; Type SG mouse not found
cmtsgu7: ret                              ;

CheckMouseTypeSG endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeAC(portAddr dx)
;
; Description:
;
;    Checks for an AceDat digitizer at the specified port address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;
; Note: AceCat digitizers are very much like Summagraphic's digitizers
;       except that they don't recognize the "puck" query.
;

cseg     segment

CheckMouseTypeAC proc near

         cmp      mouse,typeAC            ; MOUSE=AC is required to recognize
         jnz      cmtac7                  ;  an AceCat digitizer
         mov      mouse,typeErr           ;
         add      dx,msr                  ;
         in       al,dx                   ; If CTS is not active, then mouse
         MyIoDelay                        ;  won't accept commands, so it
         sub      dx,msr                  ;  can't be a type SG mouse
         test     al,msrCts               ;
         jz       cmtac7                  ;
         mov      bx,offset portMA        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         add      dx,lcr                  ;
         in       al,dx                   ; Send a break which is a "master"
         MyIoDelay                        ;  reset.  This forces the tablet
         or       al,lcrBreak             ;  into MM mode regardless of its
         out      dx,al                   ;  current state and of the
         MyIoDelay                        ;  attached cursor
         push     ax                      ;
         sub      dx,lcr                  ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         add      dx,lcr                  ;
         pop      ax                      ;
         xor      al,lcrBreak             ;
         out      dx,al                   ;
         MyIoDelay                        ;
         sub      dx,lcr                  ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      buttons,3               ;
         cmp      protocol,protoUA        ; If UIOF protocol to be used then
         jz       cmtacu1                 ;  setup is completely different
         cmp      protocol,protoUR        ;
         jz       cmtacu1                 ;
         mov      bx,offset sgSetup       ; Make sure 8 data, 1 stop, odd
         call     SendMouseString         ;  parity, eat returned ack
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset sgSize        ;
         call     SendMouseQuery          ;
         mov      cx,250                  ;
         mov      si,5                    ;
         call     ReadMouseResponse       ;
         jc       cmtac8                  ;
         push     dx                      ;
         xor      dx,dx                   ;
         mov      dh,mouseCfg[2]          ; Put X maximum position in dx
         shr      dx,1                    ;
         or       dl,mouseCfg[3]          ;
         xor      bx,bx                   ;
         mov      bh,mouseCfg[0]          ; Put Y maximum position in bx
         shr      bx,1                    ;
         mov      bl,mouseCfg[1]          ;
         cmp      bx,500                  ; Make sure table is at least one
         jc       cmtac6                  ;  inch square
         cmp      dx,500                  ;
         jc       cmtac6                  ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtac2                  ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtac3                  ;
cmtac2:  mov      rowMax,bx               ;
cmtac3:  cmp      colMax,0                ;
         jz       cmtac4                  ;
         cmp      colMax,dx               ;
         jb       cmtac5                  ;
cmtac4:  mov      colMax,dx               ;
cmtac5:  pop      dx                      ;
         or       mseSpeed,mse9600        ;
         mov      mouse,typeAC            ; Type AC mouse found
         cmp      protocol,protoMR        ;  only MM, MA and MR protocols
         jz       cmtac8                  ;  permitted
         cmp      protocol,protoMM        ;
         jz       cmtac8                  ;
         mov      protocol,protoMA        ;
         clc                              ;
         ret                              ;
cmtac6:  pop      dx                      ;
cmtac7:  stc                              ; Type SG mouse not found
cmtac8:  ret                              ;
cmtacu1: mov      bx,offset sgUiof        ;
         call     SendMouseString         ; Enable UIOF protocol
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset portUA        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset suSetup       ; Make sure 7 data, 2 stop, even
         call     SendMouseString         ;  parity, eat returned ack
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset sgXon         ; Send x-on to enable transmissions
         call     SendMouseString         ;  and eat any outstanding data
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset suSize        ;
         call     SendMouseQuery          ;
         mov      cx,250                  ;
         mov      si,8                    ;
         call     ReadMouseResponse       ;
         jc       cmtacu7                 ;
         push     dx                      ;
         mov      dl,mouseCfg[4]          ; Put X maximum position in dx
         mov      dh,mouseCfg[3]          ;
         shl      dl,2                    ;
         shl      dx,4                    ;
         or       dl,mouseCfg[5]          ;
         mov      bl,mouseCfg[1]          ;
         mov      bh,mouseCfg[0]          ; Put Y maximum position in bx
         shl      bl,2                    ;
         shl      bx,4                    ;
         or       bl,mouseCfg[2]          ;
         cmp      bx,1000                 ; Make sure table is at least one
         jc       cmtacu6                 ;  inch square
         cmp      dx,1000                 ;
         jc       cmtacu6                 ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtacu2                 ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtacu3                 ;
cmtacu2: mov      rowMax,bx               ;
cmtacu3: cmp      colMax,0                ;
         jz       cmtacu4                 ;
         cmp      colMax,dx               ;
         jb       cmtacu5                 ;
cmtacu4: mov      colMax,dx               ;
cmtacu5: pop      dx                      ;
         or       mseSpeed,mse9600        ;
         mov      mouse,typeAC            ; Type AC mouse found
         ret                              ;
cmtacu6: pop      dx                      ;
         stc                              ; Type AC mouse not found
cmtacu7: ret                              ;

CheckMouseTypeAC endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeSX(portAddr dx)
;
; Description:
;
;    Checks for SummaGraphic's FX digitizer at the specified port address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;
; Note: Summagraphics digitizers default to 500 dpi in MM mode and 1000 dpi
;       when in UIOF mode.
;

dseg     segment

sgPrssur db       'zpt05',0
suPrssur db       escape,'MPT05',0

dseg     ends

cseg     segment

CheckMouseTypeSX proc near

         cmp      mouse,typeSX            ; MOUSE=SX is required to recognize
         jnz      cmtsx7                  ;  a SummaGraphic's FX digitizer
         mov      mouse,typeErr           ;
         add      dx,msr                  ;
         in       al,dx                   ; If CTS is not active, then mouse
         MyIoDelay                        ;  won't accept commands, so it
         sub      dx,msr                  ;  can't be a type SX mouse
         test     al,msrCts               ;
         jz       cmtsx7                  ;
         mov      bx,offset portMA        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         add      dx,lcr                  ;
         in       al,dx                   ; Send a break which is a "master"
         MyIoDelay                        ;  reset.  This forces the tablet
         or       al,lcrBreak             ;  into MM mode regardless of its
         out      dx,al                   ;  current state and of the
         MyIoDelay                        ;  attached cursor
         push     ax                      ;
         sub      dx,lcr                  ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         add      dx,lcr                  ;
         pop      ax                      ;
         xor      al,lcrBreak             ;
         out      dx,al                   ;
         MyIoDelay                        ;
         sub      dx,lcr                  ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         cmp      protocol,protoUA        ; If UIOF protocol to be used then
         jz       cmtsxu1                 ;  setup is completely different
         cmp      protocol,protoUR        ;
         jz       cmtsxu1                 ;
         mov      bx,offset sgSetup       ; Make sure 8 data, 1 stop, odd
         call     SendMouseString         ;  parity, eat returned ack
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset sgSize        ;
         call     SendMouseQuery          ;
         mov      cx,250                  ;
         mov      si,5                    ;
         call     ReadMouseResponse       ;
         jc       cmtsx8                  ;
         push     dx                      ;
         xor      dx,dx                   ;
         mov      dh,mouseCfg[2]          ; Put X maximum position in dx
         shr      dx,1                    ;
         or       dl,mouseCfg[3]          ;
         xor      bx,bx                   ;
         mov      bh,mouseCfg[0]          ; Put Y maximum position in bx
         shr      bx,1                    ;
         mov      bl,mouseCfg[1]          ;
         cmp      bx,500                  ; Make sure table is at least one
         jc       cmtsx6                  ;  inch square
         cmp      dx,500                  ;
         jc       cmtsx6                  ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtsx1                  ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtsx2                  ;
cmtsx1:  mov      rowMax,bx               ;
cmtsx2:  cmp      colMax,0                ;
         jz       cmtsx3                  ;
         cmp      colMax,dx               ;
         jb       cmtsx4                  ;
cmtsx3:  mov      colMax,dx               ;
cmtsx4:  pop      dx                      ;
         mov      ax,pressure             ;
         cmp      ax,0ffffh               ; Set the pressure threshold
         jz       cmtsx5                  ;  if the PRESSURE option was
         cmp      ax,100                  ;  present
         jnc      cmtsx7                  ;
         mov      cx,10                   ;
         div      cl                      ;
         add      al,'0'                  ;
         mov      sgPrssur+3,al           ;
         add      ah,'0'                  ;
         mov      sgPrssur+4,ah           ;
         mov      bx,offset sgPrssur      ;
         call     SendMouseString         ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
cmtsx5:  mov      buttons,3               ;
         or       mseSpeed,mse9600        ;
         mov      mouse,typeSX            ; Type SX mouse found
         cmp      protocol,protoMR        ;  only MM, MA and MR protocols
         jz       cmtsx8                  ;  permitted
         cmp      protocol,protoMM        ;
         jz       cmtsx8                  ;
         mov      protocol,protoMA        ;
         clc                              ;
         ret                              ;
cmtsx6:  pop      dx                      ;
cmtsx7:  stc                              ; Type SX mouse not found
cmtsx8:  ret                              ;
cmtsxu1: mov      bx,offset sgUiof        ;
         call     SendMouseString         ; Enable UIOF protocol
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset portUA        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset suSetup       ; Make sure 7 data, 2 stop, even
         call     SendMouseString         ;  parity, eat returned ack
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset sgXon         ; Send x-on to enable transmissions
         call     SendMouseString         ;  and eat any outstanding data
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset suSize        ;
         call     SendMouseQuery          ;
         mov      cx,250                  ;
         mov      si,8                    ;
         call     ReadMouseResponse       ;
         jc       cmtsxu9                 ;
         push     dx                      ;
         mov      dl,mouseCfg[4]          ; Put X maximum position in dx
         mov      dh,mouseCfg[3]          ;
         shl      dl,2                    ;
         shl      dx,4                    ;
         or       dl,mouseCfg[5]          ;
         mov      bl,mouseCfg[1]          ;
         mov      bh,mouseCfg[0]          ; Put Y maximum position in bx
         shl      bl,2                    ;
         shl      bx,4                    ;
         or       bl,mouseCfg[2]          ;
         cmp      bx,1000                 ; Make sure table is at least one
         jc       cmtsxu7                 ;  inch square
         cmp      dx,1000                 ;
         jc       cmtsxu7                 ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtsxu2                 ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtsxu3                 ;
cmtsxu2: mov      rowMax,bx               ;
cmtsxu3: cmp      colMax,0                ;
         jz       cmtsxu4                 ;
         cmp      colMax,dx               ;
         jb       cmtsxu5                 ;
cmtsxu4: mov      colMax,dx               ;
cmtsxu5: pop      dx                      ;
         mov      ax,pressure             ;
         cmp      ax,0ffffh               ; Set the pressure threshold
         jz       cmtsxu6                 ;  if the PRESSURE option was
         cmp      ax,100                  ;  present
         jnc      cmtsxu8                 ;
         mov      cx,10                   ;
         div      cl                      ;
         add      al,'0'                  ;
         mov      suPrssur+4,al           ;
         add      ah,'0'                  ;
         mov      suPrssur+5,ah           ;
         mov      bx,offset suPrssur      ;
         call     SendMouseString         ;
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
cmtsxu6: mov      buttons,3               ;
         or       mseSpeed,mse9600        ;
         mov      mouse,typeSX            ; Type SX mouse found
         ret                              ;
cmtsxu7: pop      dx                      ;
cmtsxu8: stc                              ; Type SX mouse not found
cmtsxu9: ret                              ;

CheckMouseTypeSX endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeSM(portAddr dx)
;
; Description:
;
;    Checks for SummaGraphic's digitizer at the specified port address.  It
; is assumed that either a 4-button puck or a stylus is in use and that the
; tablet does not recognize the master reset.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;
; Note: Summagraphics digitizers default to 500 dpi in MM mode and 1000 dpi
;       when in UIOF mode.
;

cseg     segment

CheckMouseTypeSM proc near

         cmp      mouse,typeSM            ; MOUSE=SM is required to recognize
         jnz      cmtsm7                  ;  a SummaGraphic's digitizer that
         mov      mouse,typeErr           ;  is used in MM mode and which
         add      dx,msr                  ;  does not recognize maseter reset
         in       al,dx                   ;
         MyIoDelay                        ; If CTS is not active, then mouse
         sub      dx,msr                  ;  won't accept commands, so it
         test     al,msrCts               ;  can't be a type SM mouse
         jz       cmtsm7                  ;
         mov      bx,offset portMA        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      al,' '                  ;
         call     SendMouseByte           ; Do autobaud configure if needed,
         mov      cx,50000                ;  only useful if master reset does
         call     EatMouseGarbage         ;  not work
         xor      al,al                   ;
         call     SendMouseByte           ; Use a "soft" reset for tablets
         mov      cx,50000                ;  which might fail to recognize
         call     EatMouseGarbage         ;  the "master" reset
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset sgXon         ; Send x-on to enable transmissions
         call     SendMouseString         ;  and eat any outstanding data
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset sgPuck        ;
         call     SendMouseQuery          ; Request puck type, the response
         mov      cx,250                  ;  will be "CSR4" or "STYLUS"
         mov      si,9                    ;  followed by a <cr>
         call     ReadMouseResponse       ;
         mov      buttons,3               ;
         cmp      si,2                    ; If a STYLUS then we only have
         jnz      cmtsm1                  ;  two buttons, otherwise three
         mov      buttons,2               ;
cmtsm1:  mov      bx,offset sgSetup       ; Make sure 8 data, 1 stop, odd
         call     SendMouseString         ;  parity, eat returned ack
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset sgSize        ;
         call     SendMouseQuery          ;
         mov      cx,250                  ;
         mov      si,5                    ;
         call     ReadMouseResponse       ;
         jc       cmtsm8                  ;
         push     dx                      ;
         xor      dx,dx                   ;
         mov      dh,mouseCfg[2]          ; Put X maximum position in dx
         shr      dx,1                    ;
         or       dl,mouseCfg[3]          ;
         xor      bx,bx                   ;
         mov      bh,mouseCfg[0]          ; Put Y maximum position in bx
         shr      bx,1                    ;
         mov      bl,mouseCfg[1]          ;
         cmp      bx,500                  ; Make sure table is at least one
         jc       cmtsm6                  ;  inch square
         cmp      dx,500                  ;
         jc       cmtsm6                  ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtsm2                  ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtsm3                  ;
cmtsm2:  mov      rowMax,bx               ;
cmtsm3:  cmp      colMax,0                ;
         jz       cmtsm4                  ;
         cmp      colMax,dx               ;
         jb       cmtsm5                  ;
cmtsm4:  mov      colMax,dx               ;
cmtsm5:  pop      dx                      ;
         or       mseSpeed,mse9600        ;
         mov      mouse,typeSM            ; Type SM mouse found
         cmp      protocol,protoMR        ;  only MM, MA and MR protocols
         jz       cmtsm8                  ;  permitted
         cmp      protocol,protoMM        ;
         jz       cmtsm8                  ;
         mov      protocol,protoMA        ;
         clc                              ;
         ret                              ;
cmtsm6:  pop      dx                      ;
cmtsm7:  stc                              ; Type SM mouse not found
cmtsm8:  ret                              ;

CheckMouseTypeSM endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeKR(portAddr dx)
;
; Description:
;
;    Checks for Kurta digitizer at the specified port address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;
; Note: Kurta digitizers default to 500 dpi.
;

cseg     segment

CheckMouseTypeKR proc near

         cmp      mouse,typeKR            ; MOUSE=KR is required to recognize
         jnz      cmtkr6                  ;  a Kurta digitizer
         mov      mouse,typeErr           ;
         add      dx,msr                  ;
         in       al,dx                   ;
         MyIoDelay                        ; If CTS is not active, then mouse
         sub      dx,msr                  ;  won't accept commands, so it
         test     al,msrCts               ;  can't be a type KR mouse
         jz       cmtkr6                  ;
         mov      bx,offset portMA        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      al,' '                  ;
         call     SendMouseByte           ; Do autobaud configure if needed,
         mov      cx,50000                ;  only useful if master reset does
         call     EatMouseGarbage         ;  not work
         xor      al,al                   ;
         call     SendMouseByte           ; Use a "soft" reset for tablets
         mov      cx,50000                ;  which might fail to recognize
         call     EatMouseGarbage         ;  the "master" reset
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset sgXon         ; Send x-on to enable transmissions
         call     SendMouseString         ;  and eat any outstanding data
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      buttons,3               ;
         mov      bx,offset sgSize        ;
         call     SendMouseQuery          ;
         mov      cx,250                  ;
         mov      si,5                    ;
         call     ReadMouseResponse       ;
         jc       cmtkr7                  ;
         push     dx                      ;
         xor      dx,dx                   ;
         mov      dh,mouseCfg[2]          ; Put X maximum position in dx
         shr      dx,1                    ;
         or       dl,mouseCfg[3]          ;
         xor      bx,bx                   ;
         mov      bh,mouseCfg[0]          ; Put Y maximum position in bx
         shr      bx,1                    ;
         mov      bl,mouseCfg[1]          ;
         cmp      bx,500                  ; Make sure table is at least one
         jc       cmtkr5                  ;  inch square
         cmp      dx,500                  ;
         jc       cmtkr5                  ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtkr1                  ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtkr2                  ;
cmtkr1:  mov      rowMax,bx               ;
cmtkr2:  cmp      colMax,0                ;
         jz       cmtkr3                  ;
         cmp      colMax,dx               ;
         jb       cmtkr4                  ;
cmtkr3:  mov      colMax,dx               ;
cmtkr4:  pop      dx                      ;
         or       mseSpeed,mse9600        ;
         mov      mouse,typeSM            ; Type KR mouse found
         cmp      protocol,protoMR        ;  only MM, MA and MR protocols
         jz       cmtkr7                  ;  permitted
         cmp      protocol,protoMM        ;
         jz       cmtkr7                  ;
         mov      protocol,protoMA        ;
         clc                              ;
         ret                              ;
cmtkr5:  pop      dx                      ;
cmtkr6:  stc                              ; Type KR mouse not found
cmtkr7:  ret                              ;

CheckMouseTypeKR endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeSU(portAddr dx)
;
; Description:
;
;    Checks for SummaGraphic's digitizer at the specified port address.  It
; is assumed that a 16-button puck is in use and that the tablet does not
; recognize the master reset.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;
; Note: Summagraphics digitizers default to 500 dpi in MM mode and 1000 dpi
;       when in UIOF mode.
;

cseg     segment

CheckMouseTypeSU proc near

         cmp      mouse,typeSU            ; MOUSE=SU is required to recognize
         jnz      cmtsu6                  ;  a SummaGraphic's digitizer that
         mov      mouse,typeErr           ;  is used in UIOF mode and which
         add      dx,msr                  ;  does not recognize maseter reset
         in       al,dx                   ;
         MyIoDelay                        ; If CTS is not active, then mouse
         sub      dx,msr                  ;  won't accept commands, so it
         test     al,msrCts               ;  can't be a type SU mouse
         jz       cmtsu6                  ;
         mov      bx,offset portUA        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset sgXon         ; Send x-on to enable transmissions
         call     SendMouseString         ;  and eat any outstanding data
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset suReset       ;
         call     SendMouseString         ; Use a "soft" reset for tablets
         mov      cx,50000                ;  which might fail to recognize
         call     EatMouseGarbage         ;  the "master" reset
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      buttons,3               ; Only 16-button puck is allowed
         mov      bx,offset suSetup       ; Make sure 7 data, 2 stop, even
         call     SendMouseString         ;  parity, eat returned ack
         mov      cx,20000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset suSize        ;
         call     SendMouseQuery          ;
         mov      cx,250                  ;
         mov      si,8                    ;
         call     ReadMouseResponse       ;
         jc       cmtsu7                  ;
         push     dx                      ;
         mov      dl,mouseCfg[4]          ; Put X maximum position in dx
         mov      dh,mouseCfg[3]          ;
         shl      dl,2                    ;
         shl      dx,4                    ;
         or       dl,mouseCfg[5]          ;
         mov      bl,mouseCfg[1]          ;
         mov      bh,mouseCfg[0]          ; Put Y maximum position in bx
         shl      bl,2                    ;
         shl      bx,4                    ;
         or       bl,mouseCfg[2]          ;
         cmp      bx,1000                 ; Make sure table is at least one
         jc       cmtsu5                  ;  inch square
         cmp      dx,1000                 ;
         jc       cmtsu5                  ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtsu1                  ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtsu2                  ;
cmtsu1:  mov      rowMax,bx               ;
cmtsu2:  cmp      colMax,0                ;
         jz       cmtsu3                  ;
         cmp      colMax,dx               ;
         jb       cmtsu4                  ;
cmtsu3:  mov      colMax,dx               ;
cmtsu4:  pop      dx                      ;
         or       mseSpeed,mse9600        ;
         mov      mouse,typeSU            ; Type SU mouse found
         cmp      protocol,protoUR        ;  only UA and UR protocols
         jz       cmtsu7                  ;  permitted
         mov      protocol,protoUA        ;
         clc                              ;
         ret                              ;
cmtsu5:  pop      dx                      ;
cmtsu6:  stc                              ; Type SU mouse not found
cmtsu7:  ret                              ;

CheckMouseTypeSU endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeUN(portAddr dx)
;
; Description:
;
;    Checks for MicroTouch's UnMouse at the specified port address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;
; Note: The UnMouse has a fixed resolution of 1024x1024.
;

dseg     segment

unReset  db       soh,'R',cr,0
unSerial db       soh,'PN81',cr,0
unTablet db       soh,'FT',cr,0
unStream db       soh,'MS',cr,0
unFilter db       soh,'FN20',cr,0
unNoise  db       soh,etx,'F15',cr,0
unPrssur db       soh,'SE0',cr,0

dseg     ends

cseg     segment

CheckMouseTypeUN proc near

         cmp      mouse,typeUN            ; MOUSE=UN is required to recognize
         jnz      cmtun8                  ;  an UnMouse
         mov      mouse,typeErr           ;
         add      dx,msr                  ;
         in       al,dx                   ;
         MyIoDelay                        ; If CTS is not active, then mouse
         sub      dx,msr                  ;  won't accept commands, so it
         test     al,msrCts               ;  can't be a type UN mouse
         jz       cmtun8                  ;
         mov      bx,offset portUN        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset unReset       ; Reset UnMouse, this takes about
         call     SendMouseString         ;  240ms, so we wait 300.  The
         mov      cx,300                  ;  UnMouse should respond with a
         mov      si,3                    ;  three byte acknowledgement which
         call     ReadMouseResponse       ;  we ignore at this stage
         mov      bx,offset unSerial      ; Set communication parameters
         call     SendMouseString         ;  The UnMouse should respond with
         mov      bx,offset portTA        ;  a three byte acknowledgement
         call     SetSerialState          ;  which we ignore at this stage
         mov      cx,200                  ;
         mov      si,3                    ;
         call     ReadMouseResponse       ;
         mov      bx,offset unReset       ; Reset UnMouse, this takes about
         call     SendMouseString         ;  240ms, so we wait 300.  The
         mov      cx,300                  ;  UnMouse should respond with a
         mov      si,3                    ;  three byte acknowledgement.
         call     ReadMouseResponse       ;
         jc       cmtun9                  ;
         cmp      mouseCfg+2,soh          ;
         jnz      cmtun8                  ; Check for valid response
         cmp      mouseCfg+1,'0'          ;
         jnz      cmtun8                  ;
         cmp      mouseCfg+0,cr           ;
         jnz      cmtun8                  ;
         mov      bx,offset unSerial      ; Set communication parameters,
         call     SendMouseString         ;  the UnMouse should respond with
         mov      cx,200                  ;  a three byte acknowledgement.
         mov      si,3                    ;
         call     ReadMouseResponse       ;
         jc       cmtun9                  ;
         cmp      mouseCfg+2,soh          ;
         jnz      cmtun8                  ; Check for valid response
         cmp      mouseCfg+1,'0'          ;
         jnz      cmtun8                  ;
         cmp      mouseCfg+0,cr           ;
         jnz      cmtun8                  ;
         mov      bx,offset unTablet      ; Set binary tablet packet format,
         call     SendMouseString         ;  The UnMouse should respond with
         mov      cx,200                  ;  a three byte acknowledgement.
         mov      si,3                    ;
         call     ReadMouseResponse       ;
         jc       cmtun9                  ;
         cmp      mouseCfg+2,soh          ;
         jnz      cmtun8                  ; Check for valid response
         cmp      mouseCfg+1,'0'          ;
         jnz      cmtun8                  ;
         cmp      mouseCfg+0,cr           ;
         jnz      cmtun8                  ;
         mov      bx,offset unStream      ; Set stream mode, the UnMouse
         call     SendMouseString         ;  should respond with a three
         mov      cx,200                  ;  byte acknowledgement.
         mov      si,3                    ;
         call     ReadMouseResponse       ;
         jc       cmtun9                  ;
         cmp      mouseCfg+2,soh          ;
         jnz      cmtun8                  ; Check for valid response
         cmp      mouseCfg+1,'0'          ;
         jnz      cmtun8                  ;
         cmp      mouseCfg+0,cr           ;
         jnz      cmtun8                  ;
         cmp      rowMax,0                ; Save maximum row and column
         jz       cmtun1                  ;  position unless the user has
         cmp      rowMax,1024             ;  has specified a smaller maximum
         jb       cmtun2                  ;
cmtun1:  mov      rowMax,1024             ;
cmtun2:  cmp      colMax,0                ;
         jz       cmtun3                  ;
         cmp      colMax,1024             ;
         jb       cmtun4                  ;
cmtun3:  mov      colMax,1024             ;
cmtun4:  mov      ax,filter               ;
         cmp      ax,0ffffh               ; Set the number of points to be
         jz       cmtun5                  ;  discarded immediately following
         cmp      ax,99                   ;  a touch, some older revisions
         ja       cmtun8                  ;  may not handle this command
         mov      cl,10                   ;  so ignore the response
         div      cl                      ;
         add      al,'0'                  ;
         mov      unFilter+3,al           ;
         add      ah,'0'                  ;
         mov      unFilter+4,ah           ;
cmtun5:  mov      bx,offset unFilter      ;
         call     SendMouseString         ;
         mov      cx,500                  ;
         mov      si,8                    ;
         call     ReadMouseResponse       ;
         mov      ax,noise                ;
         cmp      ax,0ffffh               ; Set the frequency to control
         jz       cmtun6                  ;  noise, some older revisions
         cmp      ax,15                   ;  may not handle this command
         jb       cmtun9                  ;  so ignore the response
         cmp      ax,45                   ;
         ja       cmtun8                  ;
         mov      cl,10                   ;
         div      cl                      ;
         add      al,'0'                  ;
         mov      unNoise+3,al            ;
         add      ah,'0'                  ;
         mov      unNoise+4,ah            ;
cmtun6:  mov      bx,offset unNoise       ;
         call     SendMouseString         ;
         mov      cx,250                  ;
         mov      si,3                    ;
         call     ReadMouseResponse       ;
         mov      ax,pressure             ;
         cmp      ax,0ffffh               ; Set the pressure required to
         jz       cmtun7                  ;  recognize a button press on
         cmp      ax,3                    ;  the touch sensitive area, some
         ja       cmtun8                  ;  older revisions may not handle
         add      al,'0'                  ;  this command so ignore the
         mov      unPrssur+3,al           ;  response
cmtun7:  mov      bx,offset unPrssur      ;
         call     SendMouseString         ;
         mov      cx,250                  ;
         mov      si,3                    ;
         call     ReadMouseResponse       ;
         mov      buttons,2               ;
         or       mseSpeed,mse9600        ;
         mov      mouse,typeUN            ; Type UN mouse found
         cmp      protocol,protoTR        ;  only TA and TR protocols
         jz       cmtun9                  ;  permitted
         mov      protocol,protoTA        ;
         clc                              ;
         ret                              ;
cmtun8:  stc                              ; Type UN mouse not found
cmtun9:  ret                              ;

CheckMouseTypeUN endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeBP(portAddr dx)
;
; Description:
;
;    Checks for SummaGraphic's Bit Pad digitizer at the specified port
; address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;

dseg     segment

bpXon    db       xon,0
bpSize   db       'a',0

dseg     ends

cseg     segment

CheckMouseTypeBP proc near

         cmp      mouse,typeBP            ; MOUSE=BP is required to recognize
         jnz      cmtbp6                  ;  a SummaGraphic's Bit Pad
         mov      mouse,typeErr           ;  digitizer
         add      dx,msr                  ;
         in       al,dx                   ;
         MyIoDelay                        ; If CTS is not active, then mouse
         sub      dx,msr                  ;  won't accept commands, so it
         test     al,msrCts               ;  can't be a type BP mouse
         jz       cmtbp6                  ;
         mov      bx,offset portBA        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      al,' '                  ;
         call     SendMouseByte           ; Do autobaud configure
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         xor      al,al                   ;
         call     SendMouseByte           ; Send a "soft" reset
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset bpXon         ; Send x-on to enable transmissions
         call     SendMouseString         ;  and eat any outstanding data
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      al,'r'                  ;
         call     SendMouseByte           ; Set maximum resolution, default
         mov      al,00h                  ;  is 200 lpi.  Maximum resolution
         call     SendMouseByte           ;  is 4096 over 12 inches but
         mov      al,20h                  ;  tablet will actually set maximum
         call     sendMouseByte           ;  based on active area which is
         mov      al,00h                  ;  tablet dependent.  For example,
         call     SendMouseByte           ;  one tablet sets 4092 x 3900
         mov      al,20h                  ;  The active area may differ with
         call     SendMouseByte           ;  different resolutions, when
         call     WaitForSend             ;  setting 2400 x 2400 that same
         mov      cx,50000                ;  tablet actually sets 2400 x 2336
         call     EatMouseGarbage         ;
         mov      bx,offset bpSize        ;
         call     SendMouseQuery          ;
         mov      cx,250                  ;
         mov      si,5                    ;
         call     ReadMouseResponse       ;
         jc       cmtbp7                  ;
         push     dx                      ;
         xor      dx,dx                   ;
         mov      dh,mouseCfg[2]          ; Put X maximum position in dx
         shr      dx,2                    ;
         or       dl,mouseCfg[3]          ;
         xor      bx,bx                   ;
         mov      bh,mouseCfg[0]          ; Put Y maximum position in bx
         shr      bx,2                    ;
         mov      bl,mouseCfg[1]          ;
         cmp      bx,200                  ; Make sure table is at least one
         jc       cmtbp5                  ;  inch square
         cmp      dx,200                  ;
         jc       cmtbp5                  ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtbp1                  ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtbp2                  ;
cmtbp1:  mov      rowMax,bx               ;
cmtbp2:  cmp      colMax,0                ;
         jz       cmtbp3                  ;
         cmp      colMax,dx               ;
         jb       cmtbp4                  ;
cmtbp3:  mov      colMax,dx               ;
cmtbp4:  pop      dx                      ;
         mov      buttons,3               ;
         or       mseSpeed,mse9600        ;
         mov      mouse,typeBP            ; Type BP mouse found
         cmp      protocol,protoBR        ;  only BA and BR protocols
         jz       cmtbp7                  ;  permitted
         mov      protocol,protoBA        ;
         clc                              ;
         ret                              ;
cmtbp5:  pop      dx                      ;
cmtbp6:  stc                              ; Type BP mouse not found
cmtbp7:  ret                              ;

CheckMouseTypeBP endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeCR(portAddr dx)
;
; Description:
;
;    Checks for SummaGraphic's Bit Pad digitizer at the specified port
; address.
;
; Normal exit:
;
;    Carry clear.  Mouse found, the mouse, protocol and button fields are
; set appropriately.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    Registers ax, bx, cx, di and es are altered.
;

dseg     segment

crXon    db       xon,0
crSize   db       'a',0
crSetup  db       'h',0

dseg     ends

cseg     segment

CheckMouseTypeCR proc near

         cmp      mouse,typeCR            ; MOUSE=CR is required to recognize
         jnz      cmtcr6                  ;  a SummaGraphic's Bit Pad
         mov      mouse,typeErr           ;  digitizer in CR mode
         add      dx,msr                  ;
         in       al,dx                   ;
         MyIoDelay                        ; If CTS is not active, then mouse
         sub      dx,msr                  ;  won't accept commands, so it
         test     al,msrCts               ;  can't be a type CR mouse
         jz       cmtcr6                  ;
         mov      bx,offset portCA        ; Perform setup serial port
         call     SetSerialState          ;  initialization
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      al,' '                  ;
         call     SendMouseByte           ; Do autobaud configure
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         xor      al,al                   ;
         call     SendMouseByte           ; Send a "soft" reset
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset crXon         ; Send x-on to enable transmissions
         call     SendMouseString         ;  and eat any outstanding data
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset crSetup       ; Set resolution to 500lpi, default
         call     SendMouseString         ;  is 200 lpi.
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         mov      bx,offset crSize        ;
         call     SendMouseQuery          ;
         mov      cx,250                  ;
         mov      si,5                    ;
         call     ReadMouseResponse       ;
         jc       cmtcr7                  ;
         push     dx                      ;
         xor      dx,dx                   ;
         mov      dh,mouseCfg[2]          ; Put X maximum position in dx
         shr      dx,1                    ;
         or       dl,mouseCfg[3]          ;
         xor      bx,bx                   ;
         mov      bh,mouseCfg[0]          ; Put Y maximum position in bx
         shr      bx,1                    ;
         mov      bl,mouseCfg[1]          ;
         cmp      bx,200                  ; Make sure table is at least one
         jc       cmtcr5                  ;  inch square
         cmp      dx,200                  ;
         jc       cmtcr5                  ; Save maximum row and column
         cmp      rowMax,0                ;  position unless the user has
         jz       cmtcr1                  ;  has specified a smaller maximum
         cmp      rowMax,bx               ;
         jb       cmtcr2                  ;
cmtcr1:  mov      rowMax,bx               ;
cmtcr2:  cmp      colMax,0                ;
         jz       cmtcr3                  ;
         cmp      colMax,dx               ;
         jb       cmtcr4                  ;
cmtcr3:  mov      colMax,dx               ;
cmtcr4:  pop      dx                      ;
         mov      buttons,3               ;
         or       mseSpeed,mse9600        ;
         mov      mouse,typeCR            ; Type CR mouse found
         cmp      protocol,protoCR        ;  only CA and CR protocols
         jz       cmtcr7                  ;  permitted
         mov      protocol,protoCA        ;
         clc                              ;
         ret                              ;
cmtcr5:  pop      dx                      ;
cmtcr6:  stc                              ; Type CR mouse not found
cmtcr7:  ret                              ;

CheckMouseTypeCR endp

cseg     ends



;
; Interface:
;
;    CheckMouseTypeNM(portAddr dx)
;
; Description:
;
;    Provides "no mouse" support.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set.  No mouse found.
;
; Side effects:
;
;    No registers are altered.
;

cseg     segment

CheckMouseTypeNM proc near

         cmp      mouse,typeNM            ;
         jnz      cmtnm1                  ; MOUSE=NM is required to provide
         mov      buttons,3               ;  "no mouse" support
         mov      protocol,protoNM        ;
         clc                              ;
         ret                              ;
cmtnm1:  stc                              ;
         ret                              ;

CheckMouseTypeNM endp

cseg     ends



;
; Interface:
;
;    ReadMouseResponse(portAddr dx, timeout cx, length si)
;
; Description:
;
;    Read a response from the mouse -- time out after specified interval.
;
; Normal exit:
;
;    Carry clear and reversed response in mouseCfg, i.e. the first byte read
; will in mouseCfg[si-1] and the last byte read will be in mouseCfg[0].
; The returned value of si will be zero.
;
; Error exit:
;
;    Carry set.  Register si contains the number of bytes NOT read.
;
; Side effects:
;
;    Registers ax, bx, cx, si, di and es are altered.
;

cseg     segment

         align    4

ReadMouseResponse proc near

         call     SetupForWait            ;
rmr1:    add      dx,lsr                  ;
         in       al,dx                   ; Read response from mouse, if it
         MyIoDelay                        ;  does not respond within timeout
         sub      dx,lsr                  ;  or does not respond with all
         test     al,lsrData              ;  characters then it is not a
         jz       rmr2                    ;  valid response.
         in       al,dx                   ;
         MyIoDelay                        ;
         sub      si,1                    ;
         mov      mouseCfg[si],al         ;
         jz       rmr3                    ;
rmr2:    call     IsWaitOver              ;
         jc       rmr1                    ;
         cmp      si,1                    ;
         cmc                              ; Return carry iff time-out
rmr3:    ret                              ;

ReadMouseResponse endp

cseg     ends



;
; Interface:
;
;    SetMouseBaudRate(baudRate ax, portAddr dx)
;
; Description:
;
;    Set the baud rate for the mouse.  The rate must be: 150, 300, 600, 1200,
; 2400, 4800, 9600 or 19200.  If the requested baud rate is not supported by
; the mouse then either the highest supported baud rate which is less than
; the requested rate will be used or the lowest supported baud rate which is
; greater than the requested rate will be used.  The rate actually used will
; be placed into the baudRate field so that the uart can be programmed to the
; same rate.  If the mouse does not support any baud rates (i.e. a type X
; mouse) then the requested baud rate is saved on the assumption that the
; requested rate is supported by the mouse by default.
;
; Side effects:
;
;    Registers ax, bx and cx are altered.
;
; Note: If the baud rate is divided by 150, then the result is of the form
;       2**n and is one of the mse#### values.  Decrementing a mse#### value
;       results in a bit mask for all lower mse#### values, complementing
;       this mask results in a bit mask for all mse#### which are greater or
;       equal.  Doing a forward bit scan on a mse#### value will obtain an
;       index value which can be used for table lookup.  These tricks are
;       used below for efficient code.
;

dseg     segment

dgtRate  db       escape,'%C1N81',cr,0
flxRate  db       'b0',0

dseg     ends

cseg     segment

         align    4

SetMouseBaudRate proc near

         mov      baudRate,ax          ;
         mov      cx,150               ; Determine the baud rate to be
         div      cl                   ;  used.  Only 150, 300, 600, 1200
         test     mseSpeed,al          ;  2400, 4800, 9600 and 19200 are
         jnz      smbrSet              ;  supported.  The baud rate given
         mov      ah,al                ;  will be used if supported by the
         dec      ah                   ;  mouse, otherwise the highest
         test     mseSpeed,ah          ;  supported baud rate which is
         jnz      smbrLow              ;  less than the given rate will be
         not      ah                   ;  used, if any -- and if not then
         test     mseSpeed,ah          ;  the lowest supported baud rate
         jnz      smbrHi               ;  will be used which is greater
         ret                           ;  than the given rate.  If none
smbrLow: shr      al,1                 ;  are supported by the mouse (i.e.
         test     mseSpeed,al          ;  a type X mouse) then the given
         jz       smbrLow              ;  baud rate will be saved, but the
         jmp      short smbrSet        ;  mouse will not be programmed.
smbrHi:  add      al,al                ;
         test     mseSpeed,al          ; The baud rate to be used has now
         jz       smbrHi               ;  been determined, save it in the
smbrSet: xchg     al,cl                ;  baudrate field and save the bit
         mul      cl                   ;  mask in cl
         mov      baudRate,ax          ;
         add      dx,msr               ;
         in       al,dx                ; If CTS is not active, then mouse
         MyIoDelay                     ;  won't accept commands so assume
         sub      dx,msr               ;  that it defaults to the given
         test     al,msrCts            ;  baud rate
         jz       smbrXit              ;
         mov      al,mouse             ;
         cmp      al,typeDW            ; If a type DW, D3 or D9 mouse then
         jz       smbrDgt              ;  use the digitizer command set
         cmp      al,typeD3            ;
         jz       smbrDgt              ;
         cmp      al,typeD9            ;
         jz       smbrDgt              ;
         cmp      al,typeC             ;
         jz       smbrMse              ; If a type C, MW or VW mouse then
         cmp      al,typeMW            ;  use the mouse command set
         jz       smbrMse              ;
         cmp      al,typeVW            ;
         jz       smbrMse              ;
smbrXit: ret                           ;
smbrDgt: bsf      ax,cx                ;
         mov      ah,'7'               ; Convert bit mask into a digit,
         sub      ah,al                ;  0=19200, 1=9600, ..., 7=150 and
         mov      dgtRate+3,ah         ;  issue modified digitizer command
         mov      bx,offset dgtRate    ;
         call     SendMouseString      ;
         mov      cx,50000             ;
         call     EatMouseGarbage      ;
         ret                           ;
smbrMse: bsf      ax,cx                ;
         add      al,'n'-3             ; Convert bit mask into a letter,
         call     SendMouseCommand     ;  n=1200,o=2400,p=4800,q=9600 and
         mov      cx,50000             ;  issue mouse command
         call     EatMouseGarbage      ;
         ret                           ;

SetMouseBaudRate endp

cseg     ends



;
; Interface:
;
;    SetInportRate(rate ax, portAddr dx)
;
; Description:
;
;    Set the rate for the mouse.  The rate must be: 10, 20, 30, 40, 50, 60,
; 80, 100 or 200.  If the requested  rate is not supported by the mouse then
; either the highest supported rate which is less than the requested rate
; will be used or the lowest supported rate which is greater than the
; requested rate will be used.  The rate actually used will be placed into
; the rprtRate field for consistency.
;
; Side effects:
;
;    Registers ax and bx are altered.
;
; Note: This module assumes that Inport devices support 30, 50, 100 and 200
;       only.
;

dseg     segment

inpCodes db        inpDataE+inpHz30    ;  10Hz reporting rate requested
         db        inpDataE+inpHz30    ;  20Hz reporting rate requested
         db        inpDataE+inpHz30    ;  30Hz reporting rate requested
         db        inpDataE+inpHz30    ;  40Hz reporting rate requested
         db        inpDataE+inpHz50    ;  50Hz reporting rate requested
         db        inpDataE+inpHz50    ;  60Hz reporting rate requested
         db        inpDataE+inpHz50    ;  70Hz reporting rate requested
         db        inpDataE+inpHz50    ;  80Hz reporting rate requested
         db        inpDataE+inpHz50    ;  90Hz reporting rate requested
         db        inpDataE+inpHz100   ; 100Hz reporting rate requested
         db        inpDataE+inpHz100   ; 110Hz reporting rate requested
         db        inpDataE+inpHz100   ; 120Hz reporting rate requested
         db        inpDataE+inpHz100   ; 130Hz reporting rate requested
         db        inpDataE+inpHz100   ; 140Hz reporting rate requested
         db        inpDataE+inpHz100   ; 150Hz reporting rate requested
         db        inpDataE+inpHz100   ; 160Hz reporting rate requested
         db        inpDataE+inpHz100   ; 170Hz reporting rate requested
         db        inpDataE+inpHz100   ; 180Hz reporting rate requested
         db        inpDataE+inpHz100   ; 190Hz reporting rate requested
         db        inpDataE+inpHz200   ; 200Hz reporting rate requested

inpRates db       30,30,30,30,50,50,50,50,50,100
         db       100,100,100,100,100,100,100,100,100,200

dseg     ends

cseg     segment

         align    4

SetInportRate proc near

         mov      bx,ax                ;
         mov      al,inpMode           ;
         out      dx,al                ; Initialize Inport mouse
         mov      ax,bx                ;
         MyIoDelay                     ;
         or       ax,ax                ; If no rate given, default to 100Hz,
         jnz      sipr1                ;  this is also the hardware default
         mov      ax,100               ;
sipr1:   mov      cx,10                ; Divide rate by 10 and subtract one
         div      cl                   ;  to get a number in the range 0..19
         dec      al                   ;  that corresponds to the selected
         movzx    bx,al                ;  rate
         mov      al,inpCodes[bx]      ;
         add      dx,inpData - inpAddr ; Look up appropriate reporting rate
         out      dx,al                ;  command and program Inport
         add      dx,inpAddr - inpData ;  interface
         movzx    ax,inpRates[bx]      ;
         mov      rprtRate,ax          ;
         ret                           ;

SetInportRate endp

cseg     ends



;
; Interface:
;
;    SetPs2Rate(rate ax)
;
; Description:
;
;    Set the rate for the mouse.  The rate must be: 10, 20, 30, 40, 50, 60,
; 80, 100 or 200.  If the requested  rate is not supported by the mouse then
; either the highest supported rate which is less than the requested rate
; will be used or the lowest supported rate which is greater than the
; requested rate will be used.  The rate actually used will be placed into
; the rprtRate field for consistency.
;
; Side effects:
;
;    Registers ax and bx are altered.
;
; Note: This module assumes that PS/2 devices support 10, 20, 40, 60, 80 100
;       and 200 only.
;

dseg     segment

ps2Rates db       10,20,20,40,40,60,60,80,80,100
         db       100,100,100,100,100,100,100,100,100,200

dseg     ends

cseg     segment

         align    4

SetPs2Rate proc near

         or       ax,ax                ; If no rate given, default to 100Hz,
         jnz      spsr1                ;  this is also the hardware default
         mov      ax,100               ;
spsr1:   mov      cx,10                ; Divide rate by 10 and subtract one
         div      cl                   ;  to get a number in the range 0..19
         dec      al                   ;  that corresponds to the selected
         movzx    bx,al                ;  rate
         movzx    ax,ps2Rates[bx]      ;
         mov      rprtRate,ax          ;
         mov      al,pixRate           ;
         call     SendPs2Command       ; Set reporting rate
         mov      ax,rprtRate          ;
         call     SendPs2Command       ;
         ret                           ;

SetPs2Rate endp

cseg     ends



;
; Interface:
;
;    SetBaudRate(baudRate ax, portAddr dx)
;
; Description:
;
;    Set the baud rate for the serial port.  The rate must be: 150, 300, 600,
; 1200, 2400, 4800, 9600 or 19200.
;
; Side effects:
;
;    Registers ax and cx are altered.
;
; Note: The baud rate divided by 150 gives the mse#### value for the baud
;       rate.  The divisor latch value for 150 shifted right by the bit
;       position of the mse#### value for the baud rate gives the divisor
;       latch value for the baud rate.
;

cseg     segment

         align    4

SetBaudRate proc near

         mov      cx,150                  ;
         div      cl                      ; Calculate the divisor latch
         bsf      cx,ax                   ;  value for the baud rate.  This
         mov      ax,div150               ;  code only works if the baud
         shr      ax,cl                   ;  rate is a legal value, i.e. 150,
         mov      cx,ax                   ;  300, 600, 1200, 2400, 4800, 9600
         add      dx,lcr                  ;  or 19200.
         in       al,dx                   ;
         MyIoDelay                        ; Turn on divisor latch access bit
         push     ax                      ;
         or       al,lcrDlab              ;
         out      dx,al                   ;
         MyIoDelay                        ;
         add      dx,divMsb-lcr           ;
         mov      al,ch                   ; Set divisor latch
         out      dx,al                   ;
         MyIoDelay                        ;
         mov      al,cl                   ;
         add      dx,divLsb-divMsb        ;
         out      dx,al                   ;
         MyIoDelay                        ;
         add      dx,lcr-divLsb           ;
         pop      ax                      ; Restore original divisor latch
         out      dx,al                   ;  access bit
         MyIoDelay                        ;
         sub      dx,lcr                  ;
         mov      cx,50000                ;
         call     EatMouseGarbage         ;
         ret                              ;

SetBaudRate endp

cseg     ends



;
; Interface:
;
;    SendMouseQuery(string *ds:bx, portAddr dx)
;
; Description:
;
;    Sends a query string to a digitizer.  All query strings terminate
; with a null character.
;
; Side effects:
;
;    Registers ax, bx and cx are altered.
;

cseg     segment

         align    4

SendMouseQuery proc near

         mov      al,byte ptr [bx]        ;
sdq1:    call     SendMouseByte           ; Send the query to the digitizer
         inc      bx                      ;  one character at a time
         mov      al,byte ptr [bx]        ;
         or       al,al                   ;
         jnz      sdq1                    ;
         call     WaitForSend             ;
         ret                              ;

SendMouseQuery endp

cseg     ends



;
; Interface:
;
;    SendMouseString(string *ds:bx, portAddr dx)
;
; Description:
;
;    Sends a command string to a digitizer.  All command strings terminate
; with a null character.
;
; Side effects:
;
;    Registers ax, bx and cx are altered.
;

cseg     segment

         align    4

SendMouseString proc near

         mov      al,byte ptr [bx]        ;
sds1:    call     SendMouseByte           ; Send the command to the digitizer
         inc      bx                      ;  one character at a time, up to
         mov      al,byte ptr [bx]        ;  and including the terminating
         or       al,al                   ;  carriage return
         jnz      sds1                    ;
         call     WaitForSend             ;
         mov      cx,250                  ; Eat any garbage that might be
         call     EatMouseGarbage         ;  present
         ret                              ;

SendMouseString endp

cseg     ends



;
; Interface:
;
;    SendMouseCommand(command al, portAddr dx)
;
; Description:
;
;    Send the command to the mouse.
;
; Side effects:
;
;    Registers ax and cx are altered.
;

cseg     segment

         align    4

SendMouseCommand proc near

         push     ax                   ;
         mov      al,'*'               ;
         call     SendMouseByte        ; Send two-byte command to mouse
         pop      ax                   ;
         call     SendMouseByte        ; Wait until transmission finished
         call     WaitForSend          ;
         mov      cx,10000             ;
         call     EatMouseGarbage      ;
         ret                           ;

SendMouseCommand endp

cseg     ends



;
; Interface:
;
;    EatMouseGarbage(delay cx, portAddr dx)
;
; Description:
;
;    Eat all data and status from serial port.  The specified delay is
; approximately in microseconds with a fairly large possible variance.
;
; Side effects:
;
;    Registers ax and cx are altered.
;

cseg     segment

         align    4

EatMouseGarbage proc near

         shr      cx,1                 ;
emg1:    in       al,dx                ; Now eat garbage for a short while
         MyIoDelay                     ;  because some mice put garbage on
         add      dx,lsr               ;  the line
         in       al,dx                ;
         sub      dx,lsr               ; Delay too short to use timer, each
         MyIoDelay                     ;  i/o delay is approximately 0.5us
         dec      cx                   ;
         jnz      emg1                 ;
         ret                           ;

EatMouseGarbage endp

cseg     ends



;
; Interface:
;
;    SendMouseByte(byte al, portAddr dx)
;
; Description:
;
;    Send the byte to the mouse.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

SendMouseByte proc near

         mov      ah,al                ;
         add      dx,lsr               ;
smbPrg1: in       al,dx                ; Wait until transmitter can accept
         MyIoDelay                     ;  next character
         test     al,lsrXmtH           ;
         jz       smbPrg1              ;
         mov      al,ah                ;
         sub      dx,lsr               ; Send byte to mouse
         out      dx,al                ;
         MyIoDelay                     ;
         ret                           ;

SendMouseByte endp

cseg     ends



;
; Interface:
;
;    WaitForSend(portAddr dx)
;
; Description:
;
;    Waits for a transmission to the mouse to complete.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

WaitForSend proc near

         add      dx,lsr               ;
wfsPrg1: in       al,dx                ; Wait until transmission finished,
         MyIoDelay                     ;  the transmit shift register must
         test     al,lsrXmtS           ;  be empty
         jz       wfsPrg1              ;
         sub      dx,lsr               ;
         ret                           ;

WaitForSend endp

cseg     ends



;
; Interface:
;
;    GetCommandByte()
;
; Description:
;
;    Get the controller command byte.
;
; Normal exit:
;
;    Carry clear with controller command byte in al.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

GetCommandByte proc near

         mov      al,ps2CmdRd          ;
         call     SendPs2Control       ; Tell controller we want to read the
         jc       gcbErr               ;  command byte
         call     ReadPs2Byte          ;
gcbErr:  ret                           ;

GetCommandByte endp

cseg     ends



;
; Interface:
;
;    PutCommandByte(commandByte al)
;
; Description:
;
;    Put the controller command byte.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

PutCommandByte proc near

         push     ax                   ;
         mov      al,ps2CmdWr          ;
         call     SendPs2Control       ; Tell controller we want to write the
         pop      ax                   ;  command byte
         jc       pcbErr               ;
         call     SendPs2Data          ;
pcbErr:  ret                           ;

PutCommandByte endp

cseg     ends



;
; Interface:
;
;    SendPs2Command(ps2Command al)
;
; Description:
;
;    Send the command byte to the ps/2 auxilary device and make sure that it
; is acknowledged properly.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

SendPs2Command proc near

         call     PutPs2Mouse          ;
         jc       spcDone              ; Send control byte to auxilary device
         call     GetPs2Mouse          ;  and wait for response.  If response
         jc       spcDone              ;  is not forthcoming or if incorrect
         cmp      al,pixAck            ;  then return error
         jz       spcDone              ;
         stc                           ;
spcDone: ret                           ;

SendPs2Command endp

cseg     ends



;
; Interface:
;
;    GetPs2Mouse()
;
; Description:
;
;    Read a byte from the ps/2 mouse.
;
; Normal exit:
;
;    Carry clear with byte in al.
;
; Error exit:
;
;    Carry set with al undefined.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

GetPs2Mouse proc near

         push     cx                   ;
         mov      ah,63                ;
gpmRedo: xor      cx,cx                ;
gpmLoop: in       al,ps2Stat           ; Wait for a byte to become available
         MyIoDelay                     ;  only auxilary bytes are of interest
         and      al,ps2AxFl+ps2Otpt   ;
         cmp      al,ps2AxFl+ps2Otpt   ;
         jz       gpmOk                ;
         loop     gpmLoop              ;
         dec      ah                   ;
         jnz      gpmRedo              ;
         pop      cx                   ;
         stc                           ;
         ret                           ;
gpmOk:   pop      cx                   ;
         in       al,ps2Data           ; Read byte from mouse
         clc                           ;
         ret                           ;

GetPs2Mouse endp

cseg     ends



;
; Interface:
;
;    PutPs2Mouse(mouseData al)
;
; Description:
;
;    Write a byte to the ps/2 mouse.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

PutPs2Mouse proc near

         push     ax                   ;
         mov      al,ps2WrAux          ;
         call     SendPs2Control       ; Tell controller we want to write the
         pop      ax                   ;  auxilary device
         jc       ppmErr               ;
         call     SendPs2Data          ;
ppmErr:  ret                           ;

PutPs2Mouse endp

cseg     ends



;
; Interface:
;
;    ReadPs2Byte()
;
; Description:
;
;    Read a byte from the ps/2 keyboard/auxilary controller.
;
; Normal exit:
;
;    Carry clear with byte in al.
;
; Error exit:
;
;    Carry set with al undefined.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

ReadPs2Byte proc near

         push     cx                   ;
         xor      cx,cx                ;
rpbLoop: in       al,ps2Stat           ; Wait for a byte to become available
         MyIoDelay                     ;
         test     al,ps2Otpt           ;
         loopz    rpbLoop              ;
         pop      cx                   ;
         jz       rpbErr               ;
         in       al,ps2Data           ; Read byte from controller
         clc                           ;
         ret                           ;
rpbErr:  stc                           ; Error if no data available
         ret                           ;

ReadPs2Byte endp

cseg     ends



;
; Interface:
;
;    SendPs2Data(dataByte al)
;
; Description:
;
;    Send the data byte to the ps/2 keyboard/auxilary controller.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

SendPs2Data proc near

         push     cx                   ;
         mov      ah,al                ; The controller might be busy, so
         xor      cx,cx                ;  wait for a while if either the
spbLoop: in       al,ps2Stat           ;  input or output buffers are in use
         MyIoDelay                     ;
         test     al,ps2Inpt+ps2Otpt   ;
         loopnz   spbLoop              ;
         pop      cx                   ;
         jnz      spbErr               ;
         mov      al,ah                ;
         out      ps2Data,al           ; Send data byte to controller
         clc                           ;
         ret                           ;
spbErr:  stc                           ; Error if they never become free
         ret                           ;

SendPs2Data endp

cseg     ends



;
; Interface:
;
;    SendPs2Control(controlByte al)
;
; Description:
;
;    Send the control byte to the ps/2 keyboard/auxilary controller.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

SendPs2Control proc near

         push     cx                   ;
         mov      ah,al                ; The controller might be busy, so
         xor      cx,cx                ;  wait for a while if either the
spcLoop: in       al,ps2Stat           ;  input or output buffers are in use
         MyIoDelay                     ;
         test     al,ps2Inpt+ps2Otpt   ;
         loopnz   spcLoop              ;
         pop      cx                   ;
         jnz      spcErr               ;
         mov      al,ah                ;
         out      ps2Ctrl,al           ; Send control byte to controller
         clc                           ;
         ret                           ;
spcErr:  stc                           ; Error if they never become free
         ret                           ;

SendPs2Control endp

cseg     ends



;
; Interface:
;
;    EnableKeyboardIrq()
;
; Description:
;
;    Enables device interrupts from keyboard.
;
; Exit:
;
;    Carry clear.
;
; Side effects:
;
;    Stack is clean, register ax is altered.
;

cseg     segment

         align    4

EnableKeyboardIrq proc near

         pushf                            ;
         cli                              ; Disable cpu interrupts
         in       al,mstr8259             ;
         and      al,enableKeybrd         ; Enable mouse interrupts at 8259
         out      mstr8259,al             ;
         MyIoDelay                        ;
         popf                             ; Restore previous cpu interrupt
         ret                              ;  state

EnableKeyboardIrq endp

cseg     ends



;
; Interface:
;
;    DisableKeyboardIrq()
;
; Description:
;
;    Disables device interrupts from keyboard.
;
; Exit:
;
;    Carry clear.
;
; Side effects:
;
;    Stack is clean, register ax is altered.
;

cseg     segment

         align    4

DisableKeyboardIrq proc near

         pushf                                  ;
         cli                                    ; Disable cpu interrupts
         in       al,mstr8259                   ;
         or       al,disableKeybrd              ; Disable interrupts at 8259
         out      mstr8259,al                   ;
         MyIoDelay                              ; Restore previous cpu
         popf                                   ;  interrupt state
         ret                                    ;

DisableKeyboardIrq endp

cseg     ends



;
; Interface:
;
;    SetupForWait(milliSeconds cx)
;
; Description:
;
;    Setup up to delay the specified number of milliseconds.
;
; Side effects:
;
;    Registers bx, cx, di and es are setup for the delay -- these must be
; preserved until the delay is complete.
;

cseg     segment

         align    4

SetupForWait proc near

         xor      bx,bx                               ; Setup es:di to address the
         les      di,globalInfo                       ;  millisecond counter in the
         cli                                          ;  global information segment
         add      cx,word ptr es:[di].SIS_MsCount     ;
         adc      bx,word ptr es:[di].SIS_MsCount+2   ; Setup bx:cx to contain the
         sti                                          ;  ending time
         ret                                          ;

SetupForWait endp

cseg     ends



;
; Interface:
;
;    IsWaitOver()
;
; Description:
;
;    Return flags indicating completion of delay.
;
; Exit: Carry set if delay not completed otherwise clear.
;
; Note: SetUpWait() must be called prior to calling this procedure.  The
;       registers cx, bx, di and es set by that routine must be preserved
;       for this routine.
;

cseg     segment

         align    4

IsWaitOver proc near

         cli                                        ;
         cmp      word ptr es:[di].SIS_MsCount+2,bx ; Compare ending time to
         jnz      iwoDone                           ;  millisecond counter,
         cmp      word ptr es:[di].SIS_MsCount,cx   ;  return comparision
iwoDone: sti                                        ;  flags
         ret                                        ;

IsWaitOver endp

cseg     ends



;
; Interface:
;
;    CheckOptions()
;
; Description:
;
;    Checks the command line options on the line starting at the first
; character following the device driver name.
;
; Normal exit:
;
;    Carry clear.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Registers ax, bx, cx, dx, si, di and es are altered.  The line is
; converted to upper case.
;

cseg     segment

         align    4

CheckOptions proc near

         les      di,es:[bx].InitpBPB     ; Get pointer to config.sys line
         mov      si,di                   ;  then advance pointer past the
chkOpt0: mov      al,byte ptr es:[di]     ;  last backslash in the line
         cmp      al,'a'                  ;
         jb       chkOpt1                 ; Convert all lower case characters
         cmp      al,'z'                  ;  to upper case
         ja       chkOpt1                 ;
         add      al,'A'-'a'              ; If string ends with cr or lf
         mov      byte ptr es:[di],al     ;  then plug in a zero for
         inc      di                      ;  consistent termination
         jmp      short chkOpt0           ;
chkOpt1: or       al,al                   ;
         jz       chkOpt2                 ;
         cmp      al,cr                   ;
         jz       chkOpt2                 ;
         cmp      al,lf                   ;
         jz       chkOpt2                 ;
         inc      di                      ;
         cmp      al,'\'                  ;
         jnz      chkOpt0                 ;
         mov      si,di                   ;
         jmp      short chkOpt0           ;
chkOpt2: mov      byte ptr es:[di],0      ; es:di is now pointing to device
         mov      di,si                   ;  device driver name
chkOpt3: mov      al,byte ptr es:[di]     ;
         or       al,al                   ;
         jz       chkOpt4                 ;
         inc      di                      ; Skip until space, tab or end of
         cmp      al,' '                  ;  string found
         jz       chkOpt4                 ;
         cmp      al,tab                  ;
         jnz      chkOpt3                 ;
chkOpt4: call     FindNameOption          ; Handle NAME=xx..xx option, set
         jc       chkErr                  ;  device driver name in the header
         call     FindComOption           ; Handle COM=# option, set
         jc       chkErr                  ;  deviceData.portAddr
         mov      deviceData.portAddr,ax  ;
         call     FindPortOption          ; Handle PORT=# option, set
         jc       chkErr                  ;  deviceData.portAddr
         or       ax,ax                   ;
         jz       chkOpt5                 ;
         mov      deviceData.portAddr,ax  ;
chkOpt5: call     FindIrqOption           ; Handle IRQ=# option, set
         jc       chkErr                  ;  deviceData.irq
         mov      deviceData.irq,al       ;
         call     FindBaudRateOption      ; Handle BAUD=# option, set
         jc       chkErr                  ;  baudRate field accordingly
         mov      baudRate,ax             ;
         call     FindUartOption          ; Handle UART=# option, set
         jc       chkErr                  ;  uartType field accordingly
         mov      uartType,al             ;
         call     FindBufferedOption      ; Handle BUFFERED option, set
         jc       chkErr                  ;  buffered flag accordingly
         mov      buffered,al             ;
         call     FindShareOption         ; Handle SHARE option, set
         jc       chkErr                  ;  shared flag accordingly
         mov      shared,al               ;
         call     FindMouseOption         ; Handle MOUSE option, set
         jc       chkErr                  ;  mouse field accordingly
         mov      mouse,al                ;
         call     FindProtocolOption      ; Handle PROTOCOL option, set
         jc       chkErr                  ;  protocol field accordingly
         mov      protocol,al             ;
         call     FindButtonsOption       ; Handle BUTTONS=# option, set
         jc       chkErr                  ;  deviceData.NumButt if the
         cmp      deviceData.numButt,al   ;  number of buttons does not
         jc       chkOpt6                 ;  increase the current number
         mov      deviceData.numButt,al   ;
chkOpt6: call     FindOrderOption         ; Handle ORDER=# option, btnOrder
         jc       chkErr                  ;  table is constructed
         call     FindReverseColumnOption ;
         jc       chkErr                  ; Handle REVERSEX option, set
         mov      colRvFlg,ax             ;  colRvFlg accordingly
         call     FindReverseRowOption    ;
         jc       chkErr                  ; Handle REVERSEY option, set
         mov      rowRvFlg,ax             ;  rowRvFlg accordingly
         call     FindFlipOption          ;
         jc       chkErr                  ; Handle FLIPXY option, set
         mov      flipFlag,ax             ;  flipFlag accordingly
         call     FindResponseOption      ;
         jc       chkErr                  ; Handle RESPONSE=# option, set
         mov      response,ah             ;  response value and rounding
         xor      ah,ah                   ;  value
         mov      rounding,ax             ;
         call     FindReportingRateOption ; Handle RATE=# option, set
         jc       chkErr                  ;  rprtRate field accordingly
         mov      rprtRate,ax             ;
         call     FindDpiOption           ;
         jc       chkErr                  ; Handle DPI=# option, set
         mov      deviceData.numMics,al   ;  deviceData.numMics
         call     FindPressureOption      ;
         jc       chkErr                  ; Handle PRESSURE=# option,
         mov      pressure,ax             ;  set pressure appropriately
         call     FindNoiseOption         ;
         jc       chkErr                  ; Handle NOISE=# option,
         mov      noise,ax                ;  set noise appropriately
         call     FindFilterOption        ;
         jc       chkErr                  ; Handle FILTER=# option,
         mov      filter,ax               ;  set filter appropriately
         call     FindXOption             ;
         jc       chkErr                  ;
         mov      colMin,dx               ; Handle X=#..# option, set colMin
         mov      colMax,ax               ;  and colMax appropriately
         call     FindYOption             ;
         jc       chkErr                  ; Handle Y=#..# option, set rowMin
         mov      rowMin,dx               ;  and rowMax appropriately
         mov      rowMax,ax               ;
         call     FindMarginOption        ; Handle MARGIN=# option, set
         jc       chkErr                  ;  margin fields accordingly
         mov      colMargn,ax             ;
         mov      rowMargn,ax             ;
chkerr:  ret                              ;

CheckOptions endp

cseg     ends



;
; Interface:
;
;    FindNameOption()
;
; Description:
;
;    If the "NAME" option is present then set the device driver name.  The
; name must be between 1 and 7 characters in length.
;
; Normal exit:
;
;    Carry clear and name field changed.
;
; Error exit:
;
;    Carry set, name field undefined.
;
; Side effects:
;
;    Registers ax, bx and cx are altered.
;

cseg     segment

         align    4

FindNameOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
namChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'N', if end of
         cmp      al,'N'                     ;  string then keep default name
         je       namChk2                    ;
         or       al,al                      ;
         jnz      namChk1                    ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
namChk2: cmp      byte ptr es:[di+1],'A'     ; Once 'N' is found check for
         jnz      namChk1                    ;  "AME"
         cmp      byte ptr es:[di+2],'M'     ;
         jnz      namChk1                    ;
         cmp      byte ptr es:[di+3],'E'     ;
         jnz      namChk1                    ;
         add      di,4                       ; "NAME" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      namErr                     ;
         inc      di                         ;
         call     SkipWhiteSpace             ;

         mov      bx,offset Header + 10      ;
         mov      word ptr [bx],'  '         ; Clear the name field to all
         mov      word ptr [bx+2],'  '       ;  spaces
         mov      word ptr [bx+4],'  '       ;
         mov      word ptr [bx+6],'  '       ;

         xor      cx,cx                      ;
namChk3: mov      al,byte ptr es:[di]        ; Copy up to 7 characters to
         inc      di                         ;  the name
         or       al,al                      ;
         jz       namChk4                    ;
         cmp      al,' '                     ;
         jz       namChk4                    ;
         cmp      al,tab                     ;
         jz       namChk4                    ;
         mov      byte ptr [bx],al           ;
         inc      bx                         ;
         inc      cx                         ;
         cmp      cx,8                       ;
         jb       namChk3                    ;

namErr:  xor      ax,ax                      ;
         stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

namChk4: or       cx,cx                      ;
         jz       namErr                     ; Add a final $ to the name
         mov      byte ptr [bx],'$'          ;  and return
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;

FindNameOption endp

cseg     ends



;
; Interface:
;
;    FindComOption()
;
; Description:
;
;    Returns the serial port to be used.  If the "COM=" option is present
; then return the associated port address contained in the BIOS com port
; address table.  Otherwise return zero.  The serial port must be in the
; range 1..4.
;
; Normal exit:
;
;    Carry clear and serial port number in ax.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindComOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
comChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'C', if end of
         cmp      al,'C'                     ;  string then default to 0
         je       comChk2                    ;
         or       al,al                      ;
         jnz      comChk1                    ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
comChk2: cmp      byte ptr es:[di+1],'O'     ; Once 'C' is found check for
         jnz      comChk1                    ;  "OM"
         cmp      byte ptr es:[di+2],'M'     ;
         jnz      comChk1                    ;
         add      di,3                       ; "COM" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      comErr                     ;
         inc      di                         ;
         call     SkipWhiteSpace             ;
         cmp      al,'1'                     ;
         jb       comErr                     ; "COM=" was found, so
         cmp      al,'4'                     ;  get port address
         ja       comErr                     ;
         mov      ah,byte ptr es:[di+1]      ;
         cmp      ah,' '                     ; A space, tab or end of string
         jz       comChk3                    ;  MUST terminate com number
         cmp      ah,tab                     ;
         jz       comChk3                    ;
         or       ah,ah                      ;
         jnz      comErr                     ;
comChk3: and      ax,000fh                   ;
         dec      ax                         ;
         add      ax,ax                      ; Calculate BIOS address of
         mov      di,ax                      ;  port number (0040:xxxx)
         mov      ax,biosData                ;
         mov      es,ax                      ; Get serial port address
         mov      ax,word ptr es:[di]        ;  from BIOS data area
         or       ax,ax                      ;
         jz       comErr                     ;
         pop      di                         ;
         pop      es                         ; Return port address
         clc                                 ;
         ret                                 ;
comErr:  pop      di                         ;
         pop      es                         ; Indicate error condition
         stc                                 ;  and return
         ret                                 ;

FindComOption endp

cseg     ends



;
; Interface:
;
;    FindPortOption()
;
; Description:
;
;    Returns the port to be used.  If the "PORT=" option is present then
; return that value, otherwise zero.  The port number must be in hexadecimal.
;
; Normal exit:
;
;    Carry clear and serial port number in ax.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindPortOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
portChk1:inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'P', if end of
         cmp      al,'P'                     ;  string then default to 0
         je       portChk2                   ;
         or       al,al                      ;
         jnz      portChk1                   ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
portChk2:cmp      byte ptr es:[di+1],'O'     ; Once 'P' is found check for
         jnz      portChk1                   ;  "ORT"
         cmp      byte ptr es:[di+2],'R'     ;
         jnz      portChk1                   ;
         cmp      byte ptr es:[di+3],'T'     ;
         jnz      portChk1                   ;
         add      di,4                       ; "PORT" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      portErr                    ;
         inc      di                         ; "PORT=" was found, so
         call     GetHexNum                  ;  scan # to get port address
         jc       portErr                    ;
         push     ax                         ;
         mov      al,es:[di]                 ;
         cmp      al,' '                     ; A space, tab or end of string
         jz       portChk3                   ;  MUST terminate number
         cmp      al,tab                     ;
         jz       portChk3                   ;
         or       al,al                      ;
         jnz      portBad                    ;
portChk3:pop      ax                         ;
         or       ax,ax                      ; Must be non-zero, otherwise
         jz       portErr                    ;  report error condition
         pop      di                         ;
         pop      es                         ; Return port in ax
         clc                                 ;
         ret                                 ;
portBad: pop      ax                         ;
portErr: stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindPortOption endp

cseg     ends



;
; Interface:
;
;    FindIrqOption()
;
; Description:
;
;    Returns the IRQ number if supplied on the command line.  The irq must
; be in the range 2..15.
;
; Normal exit:
;
;    Carry clear and IRQ number in ax.  Zero is returned if "IRQ=" option is
; not present.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindIrqOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
irqChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'I', if end of
         cmp      al,'I'                     ;  string then default to 0
         je       irqChk2                    ;  so it can be calculated
         or       al,al                      ;  from port address
         jnz      irqChk1                    ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
irqChk2: cmp      byte ptr es:[di+1],'R'     ; Once 'I' is found check for
         jnz      irqChk1                    ;  "RQ"
         cmp      byte ptr es:[di+2],'Q'     ;
         jnz      irqChk1                    ;
         add      di,3                       ; "IRQ" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      irqErr                     ;
         inc      di                         ; "IRQ=" was found, so
         call     GetDecNum                  ;  scan # to get irq
         jc       irqErr                     ;
         push     ax                         ;
         mov      al,es:[di]                 ;
         cmp      al,' '                     ; A space, tab or end of string
         jz       irqChk3                    ;  MUST terminate number
         cmp      al,tab                     ;
         jz       irqChk3                    ;
         or       al,al                      ;
         jnz      irqBad                     ;
irqChk3: pop      ax                         ;
         cmp      ax,2                       ; Must be in the range 2..15,
         jb       irqErr                     ;  inclusive, otherwise report
         cmp      ax,15                      ;  error condition
         ja       irqErr                     ;
         pop      di                         ;
         pop      es                         ; Return irq in ax
         clc                                 ;
         ret                                 ;
irqBad:  pop      ax                         ;
irqErr:  stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindIrqOption endp

cseg     ends



;
; Interface:
;
;    FindBaudRateOption()
;
; Description:
;
;    Returns the baud rate if supplied on the command line.  The rate must
; be 150, 300, 600, 1200, 2400, 4800, 9600 or 19200.
;
; Normal exit:
;
;    Carry clear and baud rate in ax.  Zero is returned if "BAUD=" option is
; not present.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindBaudRateOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
baudChk1:inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'B', if end of
         cmp      al,'B'                     ;  string then default to 0
         je       baudChk2                   ;
         or       al,al                      ;
         jnz      baudChk1                   ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
baudChk2:cmp      byte ptr es:[di+1],'A'     ; Once 'B' is found check for
         jnz      baudChk1                   ;  "AUD"
         cmp      byte ptr es:[di+2],'U'     ;
         jnz      baudChk1                   ;
         cmp      byte ptr es:[di+3],'D'     ;
         jnz      baudChk1                   ;
         add      di,4                       ; "AUD" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      baudErr                    ;
         inc      di                         ; "BAUD=" was found, so
         call     GetDecNum                  ;  scan # to get baud rate
         jc       baudErr                    ;
         push     ax                         ;
         mov      al,es:[di]                 ;
         cmp      al,' '                     ; A space, tab or end of string
         jz       baudChk3                   ;  MUST terminate number
         cmp      al,tab                     ;
         jz       baudChk3                   ;
         or       al,al                      ;
         jnz      baudBad                    ;
baudChk3:pop      ax                         ;
         cmp      ax,150                     ;
         jz       baudOk                     ; Must be 150, 300, 600, 1200,
         cmp      ax,300                     ;  2400, 4800, 9600 or 19200
         jz       baudOk                     ;
         cmp      ax,600                     ;
         jz       baudOk                     ;
         cmp      ax,1200                    ;
         jz       baudOk                     ;
         cmp      ax,2400                    ;
         jz       baudOk                     ;
         cmp      ax,4800                    ;
         jz       baudOk                     ;
         cmp      ax,9600                    ;
         jz       baudOk                     ;
         cmp      ax,19200                   ;
         jnz      baudErr                    ;
baudOk:  pop      di                         ; Return baud rate in ax
         pop      es                         ;
         clc                                 ;
         ret                                 ;
baudBad: pop      ax                         ;
baudErr: stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindBaudRateOption endp

cseg     ends



;
; Interface:
;
;    FindUartOption()
;
; Description:
;
;    If the "UART" option is present then return the uart type.  The type
; must be 8250, 16450, 16550, 16550A or 16552.
;
; Normal exit:
;
;    Carry clear and uart type in ax.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindUartOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
fuoChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'U', if end of
         cmp      al,'U'                     ;  string then default to
         je       fuoChk2                    ;  no FIFO buffering requested
         or       al,al                      ;
         jnz      fuoChk1                    ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
fuoChk2: cmp      byte ptr es:[di+1],'A'     ; Once 'U' is found check for
         jnz      fuoChk1                    ;  "ART"
         cmp      byte ptr es:[di+2],'R'     ;
         jnz      fuoChk1                    ;
         cmp      byte ptr es:[di+3],'T'     ;
         jnz      fuoChk1                    ;
         add      di,4                       ; "UART" was found, make
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      fuoErr                     ;
         inc      di                         ;
         call     GetDecNum                  ; Get uart type numeric portion
         jc       fuoErr                     ;
         cmp      ax,16551                   ;
         jz       fuoErr                     ;
         cmp      ax,16550                   ;
         jnz      fuoChk3                    ;
         cmp      byte ptr es:[di],'A'       ; If 16550A then replace 16550
         jnz      fuoChk3                    ;  by 16551
         inc      ax                         ;
         inc      di                         ;
fuoChk3: push     dx                         ;
         mov      dx,ax                      ;
         mov      al,byte ptr es:[di]        ;  sure that white space or end
         or       al,al                      ;  of line follows
         jz       fuoOk                      ;
         cmp      al,' '                     ;
         jz       fuoOk                      ;
         cmp      al,tab                     ;
         jnz      fuoBad                     ;
fuoOk:   mov      ax,ins8250                 ; Only 8250, 16450, 16550,
         cmp      dx,8250                    ;  16550a and 16552 are allowed
         jz       fuoDone                    ;
         mov      ax,ns16450                 ;
         cmp      dx,16450                   ;
         jz       fuoDone                    ;
         mov      ax,ns16550                 ;
         cmp      dx,16550                   ;
         jz       fuoDone                    ;
         mov      ax,ns16550a                ;
         cmp      dx,16551                   ;
         jz       fuoDone                    ;
         cmp      dx,16552                   ;
         jnz      fuoBad                     ;
         mov      ax,ns16c552                ;
fuoDone: pop      dx                         ;
         pop      di                         ; Return uart type
         pop      es                         ;
         ret                                 ;
fuoBad:  pop      dx                         ;
fuoErr:  xor      ax,ax                      ;
         stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindUartOption endp

cseg     ends



;
; Interface:
;
;    FindBufferedOption()
;
; Description:
;
;    If the "BUFFERED" option is present then return 1, otherwise 0.
;
; Normal exit:
;
;    Carry clear and buffered flag in ax.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindBufferedOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
bufChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'B', if end of
         cmp      al,'B'                     ;  string then default to
         je       bufChk2                    ;  no FIFO buffering requested
         or       al,al                      ;
         jnz      bufChk1                    ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
bufChk2: cmp      byte ptr es:[di+1],'U'     ; Once 'B' is found check for
         jnz      bufChk1                    ;  "UFFERED"
         cmp      byte ptr es:[di+2],'F'     ;
         jnz      bufChk1                    ;
         cmp      byte ptr es:[di+3],'F'     ;
         jnz      bufChk1                    ;
         cmp      byte ptr es:[di+4],'E'     ;
         jnz      bufChk1                    ;
         cmp      byte ptr es:[di+5],'R'     ;
         jnz      bufChk1                    ;
         cmp      byte ptr es:[di+6],'E'     ;
         jnz      bufChk1                    ;
         cmp      byte ptr es:[di+7],'D'     ;
         jnz      bufChk1                    ;
         add      di,8                       ; "BUFFERED" was found, make
         mov      al,byte ptr es:[di]        ;  sure that white space or end
         or       al,al                      ;  of line follows
         jz       bufOk                      ;
         cmp      al,' '                     ;
         jz       bufChk3                    ;
         cmp      al,tab                     ;
         jnz      bufErr                     ;
bufChk3: call     SkipWhiteSpace             ; Make sure that '=' does not
         cmp      al,'='                     ;  follow "BUFFERED"
         jz       bufErr                     ;
bufOk:   mov      ax,1                       ;
         clc                                 ; Return FIFO buffer requested
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
bufErr:  xor      ax,ax                      ;
         stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindBufferedOption endp

cseg     ends



;
; Interface:
;
;    FindShareOption()
;
; Description:
;
;    If the "SHARE" option is present then return 1, otherwise 0.
;
; Normal exit:
;
;    Carry clear and buffered flag in ax.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindShareOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
fsoChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'S', if end of
         cmp      al,'S'                     ;  string then default to
         je       fsoChk2                    ;  no FIFO buffering requested
         or       al,al                      ;
         jnz      fsoChk1                    ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
fsoChk2: cmp      byte ptr es:[di+1],'H'     ; Once 'S' is found check for
         jnz      fsoChk1                    ;  "HARE"
         cmp      byte ptr es:[di+2],'A'     ;
         jnz      fsoChk1                    ;
         cmp      byte ptr es:[di+3],'R'     ;
         jnz      fsoChk1                    ;
         cmp      byte ptr es:[di+4],'E'     ;
         jnz      fsoChk1                    ;
         add      di,5                       ; "SHARE" was found, make
         mov      al,byte ptr es:[di]        ;  sure that white space or end
         or       al,al                      ;  of line follows
         jz       fsoOk                      ;
         cmp      al,' '                     ;
         jz       fsoChk3                    ;
         cmp      al,tab                     ;
         jnz      fsoErr                     ;
fsoChk3: call     SkipWhiteSpace             ; Make sure that '=' does not
         cmp      al,'='                     ;  follow "SHARE"
         jz       fsoErr                     ;
fsoOk:   mov      ax,1                       ;
         clc                                 ; Return irq sharing requested
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
fsoErr:  xor      ax,ax                      ;
         stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindShareOption endp

cseg     ends



;
; Interface:
;
;    FindMouseOption()
;
; Description:
;
;    Returns the type of mouse to be used.  If the "MOUSE=" option is present
; then return that value, otherwise default to zero.  The supported mice are:
; AC, B, BP, C, CR, DW, D3, D9, FX, I, KR, M, MS, MW, VW, NM, PS, S, SG, SM,
; SU, SX, V, UN and *.
;
; Normal exit:
;
;    Carry clear and protocol in ax.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindMouseOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
mseChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'M', if end of string
         cmp      al,'M'                     ;  then default to zero
         je       mseChk2                    ;
         or       al,al                      ;
         jnz      mseChk1                    ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
mseChk2: cmp      byte ptr es:[di+1],'O'     ; Once 'M' is found check for
         jnz      mseChk1                    ;  "OUSE"
         cmp      byte ptr es:[di+2],'U'     ;
         jnz      mseChk1                    ;
         cmp      byte ptr es:[di+3],'S'     ;
         jnz      mseChk1                    ;
         cmp      byte ptr es:[di+4],'E'     ;
         jnz      mseChk1                    ;
         add      di,5                       ; "MOUSE" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      mseErr                     ;
         inc      di                         ;
         call     SkipWhiteSpace             ; "MOUSE =" was found, so
         cmp      al,'M'                     ;  check for AC, B, BP, C, CR,
         jz       mseChk3                    ;  I, M, MS, MW, NM, VW, D, FX,
         cmp      al,'D'                     ;  KR, PS, S, SG, SM, SU, SX,
         jz       mseChk4                    ;  V, UN or *
         cmp      al,'V'                     ;
         jz       mseChk5                    ;
         cmp      al,'S'                     ;
         jz       mseChk6                    ;
         cmp      al,'F'                     ;
         jz       mseChk7                    ;
         cmp      al,'P'                     ;
         jz       mseChk8                    ;
         cmp      al,'K'                     ;
         jz       mseChk9                    ;
         cmp      al,'U'                     ;
         jz       mseChk10                   ;
         cmp      al,'B'                     ;
         jz       mseChk11                   ;
         cmp      al,'C'                     ;
         jz       mseChk12                   ;
         cmp      al,'N'                     ;
         jz       mseChk13                   ;
         cmp      al,'A'                     ;
         jz       mseChk14                   ;
         mov      ah,al                      ;
         cmp      ah,'I'                     ;
         mov      al,typeI                   ;
         jz       mseOk                      ;
         cmp      ah,'*'                     ;
         mov      al,typeX                   ;
         jz       mseOk                      ;
mseErr:  stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;
mseChk3: inc      di                         ;
         mov      ah,byte ptr es:[di]        ;
         mov      al,typeMS                  ;
         cmp      ah,'S'                     ;
         jz       mseOk                      ;
         mov      al,typeMW                  ;
         cmp      ah,'W'                     ;
         jz       mseOk                      ;
         dec      di                         ;
         mov      ax,typeM                   ;
         jmp      mseOk                      ;
mseChk4: inc      di                         ;
         mov      ah,byte ptr es:[di]        ;
         mov      al,typeDW                  ;
         cmp      ah,'W'                     ;
         jz       mseOk                      ;
         mov      al,typeD3                  ;
         cmp      ah,'3'                     ;
         jz       mseOk                      ;
         mov      al,typeD9                  ;
         cmp      ah,'9'                     ;
         jz       mseOk                      ;
         jmp      mseErr                     ;
mseChk5: cmp      byte ptr es:[di+1],'W'     ;
         mov      ax,typeV                   ;
         jnz      mseOk                      ;
         inc      di                         ;
         mov      ax,typeVW                  ;
         jmp      mseOk                      ;
mseChk6: inc      di                         ;
         mov      ah,byte ptr es:[di]        ;
         mov      al,typeSG                  ;
         cmp      ah,'G'                     ;
         jz       mseOk                      ;
         mov      al,typeSM                  ;
         cmp      ah,'M'                     ;
         jz       mseOk                      ;
         mov      al,typeSU                  ;
         cmp      ah,'U'                     ;
         jz       mseOk                      ;
         mov      al,typeSX                  ;
         cmp      ah,'X'                     ;
         jz       mseOk                      ;
         dec      di                         ;
         mov      ax,typeS                   ;
         jmp      short mseOk                ;
mseChk7: cmp      byte ptr es:[di+1],'X'     ;
         jnz      mseErr                     ;
         inc      di                         ;
         mov      ax,typeFX                  ;
         jmp      short mseOk                ;
mseChk8: cmp      byte ptr es:[di+1],'S'     ;
         jnz      mseErr                     ;
         inc      di                         ;
         mov      ax,typePS                  ;
         jmp      short mseOk                ;
mseChk9: cmp      byte ptr es:[di+1],'R'     ;
         jnz      mseErr                     ;
         inc      di                         ;
         mov      ax,typeKR                  ;
         jmp      short mseOk                ;
mseChk10:cmp      byte ptr es:[di+1],'N'     ;
         jnz      mseErr                     ;
         inc      di                         ;
         mov      ax,typeUN                  ;
         jmp      short mseOk                ;
mseChk11:cmp      byte ptr es:[di+1],'P'     ;
         mov      ax,typeB                   ;
         jnz      mseOk                      ;
         inc      di                         ;
         mov      ax,typeBP                  ;
         jmp      short mseOk                ;
mseChk12:cmp      byte ptr es:[di+1],'R'     ;
         mov      ax,typeC                   ;
         jnz      mseOk                      ;
         inc      di                         ;
         mov      ax,typeCR                  ;
         jmp      short mseOk                ;
mseChk13:cmp      byte ptr es:[di+1],'M'     ;
         jnz      mseErr                     ;
         inc      di                         ;
         mov      ax,typeNM                  ;
         jmp      short mseOk                ;
mseChk14:cmp      byte ptr es:[di+1],'C'     ;
         jnz      mseErr                     ;
         inc      di                         ;
         mov      ax,typeAC                  ;
mseOk:   inc      di                         ;
         mov      ah,es:[di]                 ; Make sure that white space or
         cmp      ah,' '                     ;  end of line follows the
         jz       mseDone                    ;  specified protocol
         cmp      ah,tab                     ;
         jz       mseDone                    ;
         or       ah,ah                      ;
         jnz      mseErr                     ;
mseDone: pop      di                         ;
         pop      es                         ;
         ret                                 ;

FindMouseOption endp

cseg     ends



;
; Interface:
;
;    FindProtocolOption()
;
; Description:
;
;    Returns the communications protocol to be used.  If the "PROTOCOL="
; option is present then return that value, otherwise default to zero.  The
; supported protocols are: 3B, 5B, BA, BR, BS, CA, CR, FA, FR, HA, HR, IN,
; MA, MI, MM, MP, MR, RE, PS, TA, TR, UA and UR.
;
; Normal exit:
;
;    Carry clear and protocol in ax.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindProtocolOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
prtChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'P', if end of string
         cmp      al,'P'                     ;  then default to zero
         je       prtChk2                    ;
         or       al,al                      ;
         jnz      prtChk1                    ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
prtChk2: cmp      byte ptr es:[di+1],'R'     ; Once 'P' is found check for
         jnz      prtChk1                    ;  "ROTOCOL"
         cmp      byte ptr es:[di+2],'O'     ;
         jnz      prtChk1                    ;
         cmp      byte ptr es:[di+3],'T'     ;
         jnz      prtChk1                    ;
         cmp      byte ptr es:[di+4],'O'     ;
         jnz      prtChk1                    ;
         cmp      byte ptr es:[di+5],'C'     ;
         jnz      prtChk1                    ;
         cmp      byte ptr es:[di+6],'O'     ;
         jnz      prtChk1                    ;
         cmp      byte ptr es:[di+7],'L'     ;
         jnz      prtChk1                    ;
         add      di,8                       ; "PROTOCOL" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      prtErr                     ;
         inc      di                         ;
         call     SkipWhiteSpace             ; "PROTOCOL =" was found, so
         cmp      al,'M'                     ;  check for 3B, 5B, BA, BR, BS,
         jz       prtChk3                    ;  CA, CR, IN, MI, MP, MM, RE,
         cmp      al,'5'                     ;  HA, HR, MA, MR, FA, FR, PS,
         jz       prtChk4                    ;  TA, TR, UA or UR
         cmp      al,'3'                     ;
         jz       prtChk5                    ;
         cmp      al,'H'                     ;
         jz       prtChk6                    ;
         cmp      al,'B'                     ;
         jz       prtChk7                    ;
         cmp      al,'I'                     ;
         jz       prtChk8                    ;
         cmp      al,'F'                     ;
         jz       prtChk9                    ;
         cmp      al,'P'                     ;
         jz       prtChk10                   ;
         cmp      al,'T'                     ;
         jz       prtChk11                   ;
         cmp      al,'U'                     ;
         jz       prtChk12                   ;
         cmp      al,'C'                     ;
         jz       prtChk13                   ;
         cmp      al,'R'                     ;
         jnz      prtErr                     ;
         cmp      byte ptr es:[di+1],'E'     ;
         jnz      prtErr                     ;
         mov      ax,protoRE                 ;
         jmp      prtOk                      ;
prtChk3: mov      ax,protoMI                 ;
         cmp      byte ptr es:[di+1],'I'     ;
         jz       prtOk                      ;
         mov      ax,protoMP                 ;
         cmp      byte ptr es:[di+1],'P'     ;
         jz       prtOk                      ;
         mov      ax,protoMM                 ;
         cmp      byte ptr es:[di+1],'M'     ;
         jz       prtOk                      ;
         mov      ax,protoMA                 ;
         cmp      byte ptr es:[di+1],'A'     ;
         jz       prtOk                      ;
         mov      ax,protoMR                 ;
         cmp      byte ptr es:[di+1],'R'     ;
         jz       prtOk                      ;
prtErr:  stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;
prtChk4: cmp      byte ptr es:[di+1],'B'     ;
         jnz      prtErr                     ;
         mov      ax,proto5B                 ;
         jmp      prtOk                      ;
prtChk5: cmp      byte ptr es:[di+1],'B'     ;
         jnz      prtErr                     ;
         mov      ax,proto3B                 ;
         jmp      prtOk                      ;
prtChk6: mov      ax,protoHA                 ;
         cmp      byte ptr es:[di+1],'A'     ;
         jz       prtOk                      ;
         cmp      byte ptr es:[di+1],'R'     ;
         jnz      prtErr                     ;
         mov      ax,protoHR                 ;
         jmp      prtOk                      ;
prtChk7: mov      ax,protoBA                 ;
         cmp      byte ptr es:[di+1],'A'     ;
         jz       prtOk                      ;
         mov      ax,protoBR                 ;
         cmp      byte ptr es:[di+1],'R'     ;
         jz       prtOk                      ;
         cmp      byte ptr es:[di+1],'S'     ;
         jnz      prtErr                     ;
         mov      ax,protoBS                 ;
         jmp      short prtOk                ;
prtChk8: cmp      byte ptr es:[di+1],'N'     ;
         jnz      prtErr                     ;
         mov      ax,protoIN                 ;
         jmp      short prtOk                ;
prtChk9: mov      ax,protoFA                 ;
         cmp      byte ptr es:[di+1],'A'     ;
         jz       prtOk                      ;
         cmp      byte ptr es:[di+1],'R'     ;
         jnz      prtErr                     ;
         mov      ax,protoFR                 ;
         jmp      short prtOk                ;
prtChk10:cmp      byte ptr es:[di+1],'S'     ;
         jnz      prtErr                     ;
         mov      ax,protoPS                 ;
         jmp      short prtOk                ;
prtChk11:mov      ax,protoTA                 ;
         cmp      byte ptr es:[di+1],'A'     ;
         jz       prtOk                      ;
         cmp      byte ptr es:[di+1],'R'     ;
         jnz      prtErr                     ;
         mov      ax,protoTR                 ;
         jmp      short prtOk                ;
prtChk12:mov      ax,protoUA                 ;
         cmp      byte ptr es:[di+1],'A'     ;
         jz       prtOk                      ;
         cmp      byte ptr es:[di+1],'R'     ;
         jnz      prtErr                     ;
         mov      ax,protoUR                 ;
         jmp      short prtOk                ;
prtChk13:mov      ax,protoCA                 ;
         cmp      byte ptr es:[di+1],'A'     ;
         jz       prtOk                      ;
         cmp      byte ptr es:[di+1],'R'     ;
         jnz      prtErr                     ;
         mov      ax,protoCR                 ;
prtOk:   inc      di                         ; Make sure that white space or
         inc      di                         ;  end of line follows the
         mov      ah,es:[di]                 ;  specified protocol
         cmp      ah,' '                     ;
         jz       prtDone                    ;
         cmp      ah,tab                     ;
         jz       prtDone                    ;
         or       ah,ah                      ;
         jnz      prtErr                     ;
prtDone: pop      di                         ;
         pop      es                         ;
         ret                                 ;

FindProtocolOption endp

cseg     ends



;
; Interface:
;
;    FindButtonsOption()
;
; Description:
;
;    Returns the number of buttons to be supported.  If the "BUTTONS="
; option is present then return that value, otherwise default to 3 buttons.
;
; Normal exit:
;
;    Carry clear and number of buttons in ax.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindButtonsOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
butChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'B', if end of
         cmp      al,'B'                     ;  string then default to
         je       butChk2                    ;  3 buttons
         or       al,al                      ;
         jnz      butChk1                    ;
         mov      ax,3                       ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
butChk2: cmp      byte ptr es:[di+1],'U'     ; Once 'B' is found check for
         jnz      butChk1                    ;  "UTTONS"
         cmp      byte ptr es:[di+2],'T'     ;
         jnz      butChk1                    ;
         cmp      byte ptr es:[di+3],'T'     ;
         jnz      butChk1                    ;
         cmp      byte ptr es:[di+4],'O'     ;
         jnz      butChk1                    ;
         cmp      byte ptr es:[di+5],'N'     ;
         jnz      butChk1                    ;
         cmp      byte ptr es:[di+6],'S'     ;
         jnz      butChk1                    ;
         add      di,7                       ; "BUTTONS" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      butErr                     ;
         inc      di                         ;
         call     SkipWhiteSpace             ; "BUTTONS =" was found, so
         cmp      al,'2'                     ;  check for '2' or '3' followed
         jb       butErr                     ;  by white space or end of
         cmp      al,'3'                     ;  string
         ja       butErr                     ;
         inc      di                         ;
         mov      ah,es:[di]                 ;
         cmp      ah,' '                     ;
         je       butOk                      ;
         cmp      ah,tab                     ;
         je       butOk                      ;
         or       ah,ah                      ;
         jnz      butErr                     ;
butOk:   and      ax,000fh                   ; Get number of buttons in ax
         pop      di                         ;  (which must be 2 or 3)
         pop      es                         ;
         ret                                 ;
butErr:  stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindButtonsOption endp

cseg     ends



;
; Interface:
;
;    FindOrderOption()
;
; Description:
;
;    Returns the phsyical buttons ordering to be used.  If the "ORDER="
; option is present then construct the btnOrder table, otherwise do nothing.
;
; Normal exit:
;
;    Carry clear and btnOrder table constructed.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Registers ax, bx, cx, dx and si are altered.
;

dseg     segment

dftOrder db       '1234567',0

dseg     ends

cseg     segment

         align    4

FindOrderOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
fooChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'O', if end of
         cmp      al,'O'                     ;  string then do nothing
         je       fooChk2                    ;
         or       al,al                      ;
         jnz      fooChk1                    ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
fooChk2: cmp      byte ptr es:[di+1],'R'     ; Once 'O' is found check for
         jnz      fooChk1                    ;  "RDER"
         cmp      byte ptr es:[di+2],'D'     ;
         jnz      fooChk1                    ;
         cmp      byte ptr es:[di+3],'E'     ;
         jnz      fooChk1                    ;
         cmp      byte ptr es:[di+4],'R'     ;
         jnz      fooChk1                    ;
         add      di,5                       ; "ORDER" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      fooErr                     ;
         inc      di                         ;
         call     SkipWhiteSpace             ; "ORDER =" was found
         cmp      al,'1'                     ;  the specified ordering to
         jb       fooErr                     ;  construct the btnOrder table
         cmp      al,'7'                     ;
         ja       fooErr                     ;
         mov      si,offset dftOrder         ;
         xor      bx,bx                      ; The specified digit string
fooChk3: cmp      bx,7                       ;  may be 1 to 7 digits long,
         jnc      fooErr                     ;  if n digits long extend digit
         mov      byte ptr [si+bx],al        ;  string with n+1..7, in order
         inc      di                         ; So, if we have ORDER=213
         inc      bx                         ;  this will become 2134567
         mov      al,byte ptr es:[di]        ;
         cmp      al,'1'                     ; This extension is handled by
         jb       fooChk4                    ;  copying the digit string
         cmp      al,'7'                     ;  over the string '1234567'
         jbe      fooChk3                    ;  with a length check
fooChk4: cmp      al,' '                     ;
         je       fooChk5                    ;
         cmp      al,tab                     ;
         je       fooChk5                    ;
         or       al,al                      ;
         jnz      fooErr                     ;
fooChk5: mov      di,offset btnOrder         ;
         xor      bx,bx                      ; Clear button ordering map
fooChk6: mov      byte ptr [di+bx],bh        ;  because we will construct
         inc      bx                         ;  a new map
         cmp      bx,128                     ;
         jc       fooChk6                    ;
         xor      dx,dx                      ; Physical button # (0..6)
fooChk7: mov      cl,byte ptr [si]           ;
         or       cl,cl                      ; Get button number (1..7) in cx
         jz       fooChk9                    ;  and exit if no more buttons
         sub      cl,'0'                     ;
         xor      bx,bx                      ; Zero button index
fooChk8: xor      ax,ax                      ;
         bt       bx,dx                      ; Clear new button bit position,
         rcl      ax,cl                      ;  get physical button bit,
         or       byte ptr [di+bx],al        ;  put into new bit position and
         inc      bx                         ;  then insert into table entry
         cmp      bx,128                     ;
         jc       fooChk8                    ;
         inc      si                         ;
         inc      dx                         ;
         jmp      fooChk7                    ;
fooChk9: cmp      btnOrder+127,127           ; Make sure all buttons were
         jnz      fooErr                     ;  specified
         pop      di                         ;
         pop      es                         ; Return, btnOrder constructed
         ret                                 ;
fooErr:  stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindOrderOption endp

cseg     ends



;
; Interface:
;
;    FindReverseColumnOption()
;
; Description:
;
;    If the "REVERSEX" option is present then return -1, otherwise 0.
;
; Normal exit:
;
;    Carry clear and reverse flag in ax.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindReverseColumnOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
frcChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'R', if end of
         cmp      al,'R'                     ;  string then default to
         je       frcChk2                    ;  no reversal
         or       al,al                      ;
         jnz      frcChk1                    ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
frcChk2: cmp      byte ptr es:[di+1],'E'     ; Once 'R' is found check for
         jnz      frcChk1                    ;  "EVERSEX"
         cmp      byte ptr es:[di+2],'V'     ;
         jnz      frcChk1                    ;
         cmp      byte ptr es:[di+3],'E'     ;
         jnz      frcChk1                    ;
         cmp      byte ptr es:[di+4],'R'     ;
         jnz      frcChk1                    ;
         cmp      byte ptr es:[di+5],'S'     ;
         jnz      frcChk1                    ;
         cmp      byte ptr es:[di+6],'E'     ;
         jnz      frcChk1                    ;
         cmp      byte ptr es:[di+7],'X'     ;
         jnz      frcChk1                    ;
         add      di,8                       ; "REVERSEX" was found, make
         mov      al,byte ptr es:[di]        ;  sure that white space or end
         or       al,al                      ;  of line follows
         jz       frcOk                      ;
         cmp      al,' '                     ;
         jz       frcChk3                    ;
         cmp      al,tab                     ;
         jnz      frcErr                     ;
frcChk3: call     SkipWhiteSpace             ; Make sure that '=' does not
         cmp      al,'='                     ;  follow "REVERSEX"
         jz       frcErr                     ;
frcOk:   mov      ax,0ffffh                  ;
         clc                                 ; Return reversal requested
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
frcErr:  xor      ax,ax                      ;
         stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindReverseColumnOption endp

cseg     ends



;
; Interface:
;
;    FindReverseRowOption()
;
; Description:
;
;    If the "REVERSEX" option is present then return -1, otherwise 0.
;
; Normal exit:
;
;    Carry clear and reverse flag in ax.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindReverseRowOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
frrChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'R', if end of
         cmp      al,'R'                     ;  string then default to
         je       frrChk2                    ;  no reversal
         or       al,al                      ;
         jnz      frrChk1                    ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
frrChk2: cmp      byte ptr es:[di+1],'E'     ; Once 'R' is found check for
         jnz      frrChk1                    ;  "EVERSEY"
         cmp      byte ptr es:[di+2],'V'     ;
         jnz      frrChk1                    ;
         cmp      byte ptr es:[di+3],'E'     ;
         jnz      frrChk1                    ;
         cmp      byte ptr es:[di+4],'R'     ;
         jnz      frrChk1                    ;
         cmp      byte ptr es:[di+5],'S'     ;
         jnz      frrChk1                    ;
         cmp      byte ptr es:[di+6],'E'     ;
         jnz      frrChk1                    ;
         cmp      byte ptr es:[di+7],'Y'     ;
         jnz      frrChk1                    ;
         add      di,8                       ; "REVERSEY" was found, make
         mov      al,byte ptr es:[di]        ;  sure that white space or end
         or       al,al                      ;  of line follows
         jz       frrOk                      ;
         cmp      al,' '                     ;
         jz       frrChk3                    ;
         cmp      al,tab                     ;
         jnz      frrErr                     ;
frrChk3: call     SkipWhiteSpace             ; Make sure that '=' does not
         cmp      al,'='                     ;  follow "REVERSEY"
         jz       frrErr                     ;
frrOk:   mov      ax,0ffffh                  ;
         clc                                 ; Return reversal requested
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
frrErr:  xor      ax,ax                      ;
         stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindReverseRowOption endp

cseg     ends



;
; Interface:
;
;    FindFlipOption()
;
; Description:
;
;    If the "FLIPXY" option is present then return 16, otherwise 0.
;
; Normal exit:
;
;    Carry clear and flip flag in ax.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindFlipOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
flpChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'F', if end of
         cmp      al,'F'                     ;  string then default to
         je       flpChk2                    ;  no reversal
         or       al,al                      ;
         jnz      flpChk1                    ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
flpChk2: cmp      byte ptr es:[di+1],'L'     ; Once 'F' is found check for
         jnz      flpChk1                    ;  "LIPXY"
         cmp      byte ptr es:[di+2],'I'     ;
         jnz      flpChk1                    ;
         cmp      byte ptr es:[di+3],'P'     ;
         jnz      flpChk1                    ;
         cmp      byte ptr es:[di+4],'X'     ;
         jnz      flpChk1                    ;
         cmp      byte ptr es:[di+5],'Y'     ;
         jnz      flpChk1                    ;
         add      di,6                       ; "FLIPXY" was found, make
         mov      al,byte ptr es:[di]        ;  sure that white space or end
         or       al,al                      ;  of line follows
         jz       flpOk                      ;
         cmp      al,' '                     ;
         jz       flpChk3                    ;
         cmp      al,tab                     ;
         jnz      flpErr                     ;
flpChk3: call     SkipWhiteSpace             ; Make sure that '=' does not
         cmp      al,'='                     ;  follow "FLIPXY"
         jz       flpErr                     ;
flpOk:   mov      ax,16                      ;
         clc                                 ; Return reversal requested
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
flpErr:  xor      ax,ax                      ;
         stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindFlipOption endp

cseg     ends



;
; Interface:
;
;    FindResponseOption()
;
; Description:
;
;    Returns the mouse responsiveness.  If the "RESPONSE=" option is present
; then return the log of that value (which MUST be -8, -4, -2, -1, 0, 1, 2,
; 4 or 8), otherwise defaulting to zero.  When negative, the result is used
; as a shift factor to reduce mouse responsiveness and when positive the
; value is used to select the mouse acceleration routine.
;
; Normal exit:
;
;    Carry clear and response in ah and rounding constant for negative
; values in al.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Registers ax and bx are altered.
;

dseg     segment

froSlow  dw       0000h,0000h,0ff01h,0,0fe03h,0,0,0,0fd07h
froFast  dw       0000h,0000h,0100h,0,0200h,0,0,0,0300h

dseg     ends

cseg     segment

         align    4

FindResponseOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
froChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'R', if end of
         cmp      al,'R'                     ;  string then default to
         je       froChk2                    ;  normal response
         or       al,al                      ;
         jnz      froChk1                    ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
froChk2: cmp      byte ptr es:[di+1],'E'     ; Once 'R' is found check for
         jnz      froChk1                    ;  "ESPONSE"
         cmp      byte ptr es:[di+2],'S'     ;
         jnz      froChk1                    ;
         cmp      byte ptr es:[di+3],'P'     ;
         jnz      froChk1                    ;
         cmp      byte ptr es:[di+4],'O'     ;
         jnz      froChk1                    ;
         cmp      byte ptr es:[di+5],'N'     ;
         jnz      froChk1                    ;
         cmp      byte ptr es:[di+6],'S'     ;
         jnz      froChk1                    ;
         cmp      byte ptr es:[di+7],'E'     ;
         jnz      froChk1                    ;
         add      di,8                       ; "RESPONSE" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      froErr                     ;
         inc      di                         ;
         call     SkipWhiteSpace             ; "RESPONSE =" was found, so
         cmp      al,'-'                     ;  save leading minus sign and
         setz     bl                         ;  check for '0', '1', '2', '4'
         and      bx,1                       ;  or '8' followed by white
         add      di,bx                      ;  space or end of string
         mov      al,byte ptr es:[di]        ;
         cmp      al,'0'                     ;
         je       froOk                      ;
         cmp      al,'1'                     ;
         je       froOk                      ;
         cmp      al,'2'                     ;
         je       froOk                      ;
         cmp      al,'4'                     ;
         je       froOk                      ;
         cmp      al,'8'                     ;
         jnz      froErr                     ;
froOk:   mov      ah,es:[di+1]               ;
         cmp      ah,' '                     ;
         je       froOkk                     ;
         cmp      ah,tab                     ;
         je       froOkk                     ;
         or       ah,ah                      ;
         jnz      froErr                     ;
froOkk:  or       bx,bx                      ;
         jz       froAccel                   ;
         mov      di,ax                      ;
         and      di,000fh                   ; Get response shift factor in
         add      di,di                      ;  ah and rounding constant in
         mov      ax,froSlow[di]             ;  al
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
froAccel:mov      di,ax                      ;
         and      di,000fh                   ; Get shift factor in ah
         add      di,di                      ;  al will be zero
         mov      ax,froFast[di]             ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
froErr:  stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindResponseOption endp

cseg     ends



;
; Interface:
;
;    FindReportingRateOption()
;
; Description:
;
;    Returns the reporting rate if supplied on the command line.  The rate
; must be: 10, 20, 30, 40, 50, 60, 80, 100 or 200.
;
; Normal exit:
;
;    Carry clear and reporting rate in ax.  Zero is returned if "RATE="
; option is not present.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindReportingRateOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
freChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'R', if end of
         cmp      al,'R'                     ;  string then default to 0
         je       freChk2                    ;
         or       al,al                      ;
         jnz      freChk1                    ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
freChk2: cmp      byte ptr es:[di+1],'A'     ; Once 'R' is found check for
         jnz      freChk1                    ;  "ATE"
         cmp      byte ptr es:[di+2],'T'     ;
         jnz      freChk1                    ;
         cmp      byte ptr es:[di+3],'E'     ;
         jnz      freChk1                    ;
         add      di,4                       ; "ATE" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      freErr                     ;
         inc      di                         ; "RATE=" was found, so
         call     GetDecNum                  ;  scan # to get reporting rate
         jc       freErr                     ;
         push     ax                         ;
         mov      al,es:[di]                 ;
         cmp      al,' '                     ; A space, tab or end of string
         jz       freChk3                    ;  MUST terminate number
         cmp      al,tab                     ;
         jz       freChk3                    ;
         or       al,al                      ;
         jnz      freBad                     ;
freChk3: pop      ax                         ;
         cmp      ax,10                      ;
         jz       freOk                      ; Must be 10, 20, 30, 40, 50,
         cmp      ax,20                      ;  60, 80, 100 or 200
         jz       freOk                      ;
         cmp      ax,30                      ;
         jz       freOk                      ;
         cmp      ax,40                      ;
         jz       freOk                      ;
         cmp      ax,50                      ;
         jz       freOk                      ;
         cmp      ax,60                      ;
         jz       freOk                      ;
         cmp      ax,80                      ;
         jz       freOk                      ;
         cmp      ax,100                     ;
         jz       freOk                      ;
         cmp      ax,200                     ;
         jnz      freErr                     ;
freOk:   pop      di                         ; Return reporting rate in ax
         pop      es                         ;
         clc                                 ;
         ret                                 ;
freBad:  pop      ax                         ;
freErr:  stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindReportingRateOption endp

cseg     ends



;
; Interface:
;
;    FindDpiOption()
;
; Description:
;
;    Returns the number of mickeys per centimeter.  If the "DPI="
; option is present then return that value divided by 2.54 is used
; (with the result rounded), otherwise default to 80 mickeys.  The
; number of "dots per inch" must be in the range 100..640.
;
; Normal exit:
;
;    Carry clear and number of mickeys per centimeter in ax.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindDpiOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
dpiChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'D', if end of
         cmp      al,'D'                     ;  string then default to
         je       dpiChk2                    ;  80 mickeys per centimeter
         or       al,al                      ;  (approximately 200 dpi)
         jnz      dpiChk1                    ;
         mov      ax,80                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
dpiChk2: cmp      byte ptr es:[di+1],'P'     ; Once 'D' is found check for
         jnz      dpiChk1                    ;  "PI"
         cmp      byte ptr es:[di+2],'I'     ;
         jnz      dpiChk1                    ;
         add      di,3                       ; "DPI" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      dpiErr                     ;
         inc      di                         ; "DPI=" was found, so
         call     GetDecNum                  ;  scan # to get dots per
         jc       dpiErr                     ;  inch
         push     cx                         ;
         mov      cx,ax                      ;
         mov      al,es:[di]                 ;
         cmp      al,' '                     ; A space, tab or end of string
         jz       dpiChk3                    ;  MUST terminate number
         cmp      al,tab                     ;
         jz       dpiChk3                    ;
         or       al,al                      ;
         jnz      dpiBad                     ;
dpiChk3: cmp      cx,640                     ; Must be in the range 100..640,
         ja       dpiBad                     ;  inclusive, otherwise report
         mov      ax,100                     ;  error condition
         cmp      cx,ax                      ;
         jb       dpiBad                     ;
         push     dx                         ; Divide "dots per inch" by 2.54
         mul      cx                         ;  to get mickeys per centimeter
         add      ax,122                     ;
         adc      dx,0                       ;
         mov      cx,254                     ;
         div      cx                         ;
         pop      dx                         ;
         pop      cx                         ; Return number of mickeys in ax
         pop      di                         ;
         pop      es                         ;
         clc                                 ;
         ret                                 ;
dpiBad:  pop      cx                         ;
dpiErr:  stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindDpiOption endp

cseg     ends



;
; Interface:
;
;    FindXOption()
;
; Description:
;
;    Returns the active horizontal range for a digitizer tablet.  This option
; has the format: X=# or X=#..# with no spaces around "..".
;
; Normal exit:
;
;    Carry clear and range in registers dx and ax.  Register ax has the
; maximum value and dx the minimum value.  Thus if ax:dx = 1000:7000 then
; the tablet's active range will be 1000..6999.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Registers ax and dx are altered.
;

cseg     segment

         align    4

FindXOption proc near

         push     es                         ;
         push     di                         ; The pointer es:di is preserved
         mov      al, byte ptr es:[di]       ;
         cmp      al,'X'                     ;
         jz       xChk2                      ;
         or       al,al                      ;
         jnz      xChk0                      ;
         xor      dx,dx                      ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
xChk0:   inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'X', if end of
         cmp      al,'X'                     ;  string then default to
         je       xChk1                      ;  zero
         or       al,al                      ;
         jnz      xChk0                      ;
         xor      dx,dx                      ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
xChk1:   mov      al,byte ptr es:[di-1]      ;
         cmp      al,'A'                     ; Make sure that 'X' is not
         jb       xChk2                      ;  embedded in another option
         cmp      al,'Z'                     ;
         jbe      xChk0                      ;
xChk2:   inc      di                         ; "X" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      xErr                       ;
         inc      di                         ; "X=" was found, so
         call     GetDecNum                  ;  scan # to get dots per
         jc       xErr                       ;  inch
         mov      dl,es:[di]                 ;
         cmp      dl,' '                     ; A space, tab or end of string
         jz       xChk3                      ;  MUST terminate number
         cmp      dl,tab                     ;
         jz       xChk3                      ;
         or       dl,dl                      ;
         jz       xChk3                      ;
         cmp      dl,'.'                     ;
         jnz      xErr                       ;
         cmp      dl,byte ptr es:[di+1]      ;
         jnz      xErr                       ;
         mov      dx,ax                      ;
         add      di,2                       ;
         mov      al,byte ptr es:[di]        ;
         cmp      al,'0'                     ;
         jb       xErr                       ;
         cmp      al,'9'                     ;
         ja       xErr                       ;
         call     GetDecNum                  ;
         cmp      dx,ax                      ;
         jnc      xErr                       ;
         pop      di                         ; If only maximum is specified
         pop      es                         ;  then minimum defaults to zero
         clc                                 ;
         ret                                 ;
xChk3:   xor      dx,dx                      ;
         pop      di                         ; If only maximum is specified
         pop      es                         ;  then minimum defaults to zero
         clc                                 ;
         ret                                 ;
xErr:    pop      di                         ;
         pop      es                         ; Indicate error condition
         stc                                 ;  and return
         ret                                 ;

FindXOption endp

cseg     ends



;
; Interface:
;
;    FindYOption()
;
; Description:
;
;    Returns the active horizontal range for a digitizer tablet.  This option
; has the format: Y=# or Y=#..# with no spaces around "..".
;
; Normal exit:
;
;    Carry clear and range in registers dx and ax.  Register ax has the
; maximum value and dx the minimum value.  Thus if ax:dx = 1000:7000 then
; the tablet's active range will be 1000..6999.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Registers ax and dx are altered.
;

cseg     segment

         align    4

FindYOption proc near

         push     es                         ;
         push     di                         ; The pointer es:di is preserved
         mov      al, byte ptr es:[di]       ;
         cmp      al,'Y'                     ;
         jz       yChk2                      ;
         or       al,al                      ;
         jnz      yChk0                      ;
         xor      dx,dx                      ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
yChk0:   inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'Y', if end of
         cmp      al,'Y'                     ;  string then default to
         je       yChk1                      ;  zero
         or       al,al                      ;
         jnz      yChk0                      ;
         xor      dx,dx                      ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
yChk1:   mov      al,byte ptr es:[di-1]      ;
         cmp      al,'A'                     ; Make sure that 'Y' is not
         jb       yChk2                      ;  embedded in another option
         cmp      al,'Z'                     ;
         jbe      yChk0                      ;
yChk2:   inc      di                         ; "Y" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      yErr                       ;
         inc      di                         ; "Y=" was found, so
         call     GetDecNum                  ;  scan # to get dots per
         jc       yErr                       ;  inch
         mov      dl,es:[di]                 ;
         cmp      dl,' '                     ; A space, tab or end of string
         jz       yChk3                      ;  MUST terminate number
         cmp      dl,tab                     ;
         jz       yChk3                      ;
         or       dl,dl                      ;
         jz       yChk3                      ;
         cmp      dl,'.'                     ;
         jnz      yErr                       ;
         cmp      dl,byte ptr es:[di+1]      ;
         jnz      yErr                       ;
         mov      dx,ax                      ;
         add      di,2                       ;
         mov      al,byte ptr es:[di]        ;
         cmp      al,'0'                     ;
         jb       yErr                       ;
         cmp      al,'9'                     ;
         ja       yErr                       ;
         call     GetDecNum                  ;
         cmp      dx,ax                      ;
         jnc      yErr                       ;
         pop      di                         ; If only maximum is specified
         pop      es                         ;  then minimum defaults to zero
         clc                                 ;
         ret                                 ;
yChk3:   xor      dx,dx                      ;
         pop      di                         ; If only maximum is specified
         pop      es                         ;  then minimum defaults to zero
         clc                                 ;
         ret                                 ;
yErr:    pop      di                         ;
         pop      es                         ; Indicate error condition
         stc                                 ;  and return
         ret                                 ;

FindYOption endp

cseg     ends



;
; Interface:
;
;    FindMarginOption()
;
; Description:
;
;    Returns the margin size if supplied on the command line.  If the row or
; column ranges are known, the specified margin must be possible.  I.e.
; min + margin < max - margin must hold.
;
; Normal exit:
;
;    Carry clear and margin size in ax.  Zero is returned if the "MARGIN="
; option is not present.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;

cseg     segment

         align    4

FindMarginOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
mrgnChk1:inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'M', if end of
         cmp      al,'M'                     ;  string then default to 0
         je       mrgnChk2                   ;
         or       al,al                      ;
         jnz      mrgnChk1                   ;
         xor      ax,ax                      ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
mrgnChk2:cmp      byte ptr es:[di+1],'A'     ; Once 'M' is found check for
         jnz      mrgnChk1                   ;  "ARGIN"
         cmp      byte ptr es:[di+2],'R'     ;
         jnz      mrgnChk1                   ;
         cmp      byte ptr es:[di+3],'G'     ;
         jnz      mrgnChk1                   ;
         cmp      byte ptr es:[di+4],'I'     ;
         jnz      mrgnChk1                   ;
         cmp      byte ptr es:[di+5],'N'     ;
         jnz      mrgnChk1                   ;
         add      di,6                       ; "MARGIN" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      mrgnErr                    ;
         inc      di                         ; "MARGIN=" was found, so
         call     GetDecNum                  ;  scan # to get margin size
         jc       mrgnErr                    ;
         push     ax                         ;
         mov      al,es:[di]                 ;
         cmp      al,' '                     ; A space, tab or end of string
         jz       mrgnChk3                   ;  MUST terminate number
         cmp      al,tab                     ;
         jz       mrgnChk3                   ;
         or       al,al                      ;
         jnz      mrgnBad                    ;
mrgnChk3:pop      ax                         ;
         cmp      rowMax,0                   ; If row minimum and maximum
         jz       mrgnChk4                   ;  are known then make sure
         push     bx                         ;  that margin is possible
         push     cx                         ;
         mov      bx,rowMin                  ;
         mov      cx,rowMax                  ;
         add      bx,ax                      ;
         sub      cx,ax                      ;
         cmp      bx,cx                      ;
         pop      cx                         ;
         pop      bx                         ;
         jnc      mrgnErr                    ;
mrgnChk4:cmp      colMax,0                   ;
         jz       mrgnChk5                   ; If column minimum and maximum
         push     bx                         ;  are known then make sure
         push     cx                         ;  that margin is possbile
         mov      bx,colMin                  ;
         mov      cx,colMax                  ;
         add      bx,ax                      ;
         sub      cx,ax                      ;
         cmp      bx,cx                      ;
         pop      cx                         ;
         pop      bx                         ;
         jnc      mrgnErr                    ;
mrgnChk5:pop      di                         ; Return margin size in ax
         pop      es                         ;
         clc                                 ;
         ret                                 ;
mrgnBad: pop      ax                         ;
mrgnErr: stc                                 ;
         pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindMarginOption endp

cseg     ends



;
; Interface:
;
;    FindPressureOption()
;
; Description:
;
;    Determines the threshold pressure for pressure sensitive buttons (as on
; a pen or touch screen).  If the "PRESSURE=" option is present then return
; the value with no range checking -- the values are device dependent.
;
; Normal exit:
;
;    Carry clear and result in ax.  The result will be 0ffffh if the
; PRESSURE option is not found.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;


cseg     segment

         align    4

FindPressureOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
prsChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'P', if end of
         cmp      al,'P'                     ;  string then default to
         je       prsChk2                    ;  0ffffh
         or       al,al                      ;
         jnz      prsChk1                    ;
         mov      ax,0ffffh                  ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
prsChk2: cmp      byte ptr es:[di+1],'R'     ; Once 'P' is found check for
         jnz      prsChk1                    ;  "RESSURE"
         cmp      byte ptr es:[di+2],'E'     ;
         jnz      prsChk1                    ;
         cmp      byte ptr es:[di+3],'S'     ;
         jnz      prsChk1                    ;
         cmp      byte ptr es:[di+4],'S'     ;
         jnz      prsChk1                    ;
         cmp      byte ptr es:[di+5],'U'     ;
         jnz      prsChk1                    ;
         cmp      byte ptr es:[di+6],'R'     ;
         jnz      prsChk1                    ;
         cmp      byte ptr es:[di+7],'E'     ;
         jnz      prsChk1                    ;
         add      di,8                       ; "PRESSURE" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      prsErr                     ;
         inc      di                         ;
         call     GetDecNum                  ; "PRESSURE=" was found, so
         jc       prsBad                     ;  scan # to get threshold
         push     ax                         ;  pressure
         mov      al,es:[di]                 ;
         cmp      al,' '                     ;
         jz       prsChk3                    ;
         cmp      al,tab                     ; A space, tab or end of string
         jz       prsChk3                    ;  MUST terminate number
         or       al,al                      ;
         jz       prsChk3                    ;
         stc                                 ;
prsChk3: pop      ax                         ;
         pop      di                         ; Indicate good option and
         pop      es                         ;  return with value in ax
         ret                                 ;
prsErr:  stc                                 ;
prsBad:  pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindPressureOption endp

cseg     ends



;
; Interface:
;
;    FindFilterOption()
;
; Description:
;
;    Determines the filter parameter for devices which can alter device
; characteristics that discard margin data points.  If the "FILTER="
; option is present then return the value with no range checking -- the
; values are device dependent.
;
; Normal exit:
;
;    Carry clear and result in ax.  The result will be 0ffffh if the
; FILTER option is not found.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;


cseg     segment

         align    4

FindFilterOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
ffoChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'F', if end of
         cmp      al,'F'                     ;  string then default to
         je       ffoChk2                    ;  0ffffh
         or       al,al                      ;
         jnz      ffoChk1                    ;
         mov      ax,0ffffh                  ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
ffoChk2: cmp      byte ptr es:[di+1],'I'     ; Once 'F' is found check for
         jnz      ffoChk1                    ;  "ILTER"
         cmp      byte ptr es:[di+2],'L'     ;
         jnz      ffoChk1                    ;
         cmp      byte ptr es:[di+3],'T'     ;
         jnz      ffoChk1                    ;
         cmp      byte ptr es:[di+4],'E'     ;
         jnz      ffoChk1                    ;
         cmp      byte ptr es:[di+5],'R'     ;
         jnz      ffoChk1                    ;
         add      di,6                       ; "FILTER" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      ffoErr                     ;
         inc      di                         ;
         call     GetDecNum                  ; "FILTER=" was found, so
         jc       ffoBad                     ;  scan # to get filter value
         push     ax                         ;
         mov      al,es:[di]                 ;
         cmp      al,' '                     ;
         jz       ffoChk3                    ;
         cmp      al,tab                     ; A space, tab or end of string
         jz       ffoChk3                    ;  MUST terminate number
         or       al,al                      ;
         jz       ffoChk3                    ;
         stc                                 ;
ffoChk3: pop      ax                         ;
         pop      di                         ; Indicate good option and
         pop      es                         ;  return with value in ax
         ret                                 ;
ffoErr:  stc                                 ;
ffoBad:  pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindFilterOption endp

cseg     ends



;
; Interface:
;
;    FindNoiseOption()
;
; Description:
;
;   Determines the noise parameter for devices which can alter device
; characteristics that affect noise or jitter.    If the "NOISE=" option
; is present then return the value with no range checking -- the values
; are device dependent.
;
; Normal exit:
;
;    Carry clear and result in ax.  The result will be 0ffffh if the
; NOISE option is not found.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Register ax is altered.
;


cseg     segment

         align    4

FindNoiseOption proc near

         push     es                         ; The pointer es:di is preserved
         push     di                         ;
         dec      di                         ; Pre-decrement for loop
fnoChk1: inc      di                         ;
         mov      al,byte ptr es:[di]        ; Scan for 'N', if end of
         cmp      al,'N'                     ;  string then default to
         je       fnoChk2                    ;  existing pressure
         or       al,al                      ;
         jnz      fnoChk1                    ;
         mov      ax,0ffffh                  ;
         pop      di                         ;
         pop      es                         ;
         ret                                 ;
fnoChk2: cmp      byte ptr es:[di+1],'O'     ; Once 'N' is found check for
         jnz      fnoChk1                    ;  "OISE"
         cmp      byte ptr es:[di+2],'I'     ;
         jnz      fnoChk1                    ;
         cmp      byte ptr es:[di+3],'S'     ;
         jnz      fnoChk1                    ;
         cmp      byte ptr es:[di+4],'E'     ;
         jnz      fnoChk1                    ;
         add      di,5                       ; "NOISE" was found, skip
         call     SkipWhiteSpace             ;  any white space and check
         cmp      al,'='                     ;  for '='
         jnz      fnoErr                     ;
         inc      di                         ;
         call     GetDecNum                  ; "NOISE=" was found, so scan #
         jc       fnoBad                     ;  to get noise parameter
         push     ax                         ;
         mov      al,es:[di]                 ;
         cmp      al,' '                     ;
         jz       fnoChk3                    ;
         cmp      al,tab                     ; A space, tab or end of string
         jz       fnoChk3                    ;  MUST terminate number
         or       al,al                      ;
         jz       fnoChk3                    ;
         stc                                 ;
fnoChk3: pop      ax                         ;
         pop      di                         ; Indicate good option and
         pop      es                         ;  return with value in ax
         ret                                 ;
fnoErr:  stc                                 ;
fnoBad:  pop      di                         ; Indicate error condition
         pop      es                         ;  and return
         ret                                 ;

FindNoiseOption endp

cseg     ends



;
; Interface:
;
;    GetDecNum(Char *es:di)
;
; Description:
;
;    Returns a decimal number supplied on the command line.
;
; Normal exit:
;
;    Carry clear, number in ax.  Register pair es:di point to first character
; following number.
;
; Error exit:
;
;    Carry set.
;
; Side effects:
;
;    Registers ax and di are altered.
;

cseg     segment

         align    4

GetDecNum proc near

         call     SkipWhiteSpace             ;
         cmp      al,'0'                     ; Decimal number MUST start with
         jc       gdnErr                     ;  a valid digit
         cmp      al,'9'                     ;
         ja       gdnErr                     ;
         push     bx                         ;
         xor      bx,bx                      ;
gdn1:    mov      al,es:[di]                 ;
         cmp      al,'0'                     ;
         jb       gdn2                       ; If not a digit then done
         cmp      al,'9'                     ;
         ja       gdn2                       ;
         inc      di                         ;
         and      ax,000fh                   ;
         add      bx,bx                      ; Multiply partial number by 10
         jc       gdnBad                     ;  and add in low digit, if
         add      ax,bx                      ;  value exceeds 65535 then
         jc       gdnBad                     ;  exit with an error
         add      bx,bx                      ;
         jc       gdnBad                     ;
         add      bx,bx                      ;
         jc       gdnBad                     ;
         add      bx,ax                      ;
         jnc      gdn1                       ;
gdnBad:  pop      bx                         ;
gdnErr:  stc                                 ;
         ret                                 ;
gdn2:    mov      ax,bx                      ;
         pop      bx                         ; Place number in ax and return
         clc                                 ;  with carry clear
         ret                                 ;

GetDecNum endp

cseg     ends



;
; Interface:
;
;    GetHexNum(Char *es:di)
;
; Description:
;
;    Returns a hex number supplied on the command line.  If the first
; character is not a valid digit or the value exceeds 65535 then the error
; exit will be taken.
;
; Normal exit:
;
;    Carry clear, number in ax.  Register pair es:di point to first character
; following number.
;
; Error exit:
;
;    Carry set.  If the first character is not a valid digit or the value
; exceeds 65535 then the error exit will be taken.
;
; Side effects:
;
;    Registers ax and di are altered.
;

cseg     segment

         align    4

GetHexNum proc near

         call     SkipWhiteSpace             ;
         cmp      al,'A'                     ; Hex number MUST start with
         jb       ghn1                       ;  a valid digit
         cmp      al,'F'                     ;
         jbe      ghn2                       ;
ghn1:    cmp      al,'0'                     ;
         jb       ghnErr                     ;
         cmp      al,'9'                     ;
         ja       ghnErr                     ;
ghn2:    push     bx                         ;
         xor      bx,bx                      ;
ghn3:    mov      al,es:[di]                 ;
         cmp      al,'A'                     ; If not a hex digit then done
         jb       ghn4                       ;
         cmp      al,'F'                     ;
         ja       ghn4                       ;
         add      al,'0'-'A'+10              ;
         jmp      short ghn5                 ;
ghn4:    cmp      al,'0'                     ;
         jb       ghn6                       ;
         cmp      al,'9'                     ;
         ja       ghn6                       ;
ghn5:    test     bx,0f000h                  ;
         jnz      ghnBad                     ;
         inc      di                         ;
         and      ax,000fh                   ;
         shl      bx,4                       ; Multiply partial number by 10
         add      bx,ax                      ;  and add in low digit
         jmp      ghn3                       ;
ghn6:    mov      ax,bx                      ;
         pop      bx                         ; Place number in ax and return
         clc                                 ;  with carry clear
         ret                                 ;
ghnBad:  pop      bx                         ;
ghnErr:  stc                                 ;
         ret                                 ;

GetHexNum endp

cseg     ends



;
; Interface:
;
;    SkipWhiteSpace(Buffer *es:[di])
;
; Description:
;
;    Advances pointer past white space.
;
; Side effects:
;
;    Stack is clean, registers ax and di altered.
;

cseg     segment

         align    4

SkipWhiteSpace proc near

         dec      di                   ; Pre-decrement for loop
skipChr: inc      di                   ;
         mov      al,byte ptr es:[di]  ; Get next character, if it is
         cmp      al,' '               ;  a space or tab then loop
         jz       skipChr              ;
         cmp      al,tab               ;
         jz       skipChr              ; Return pointing past white space
         ret                           ;

SkipWhiteSpace endp

cseg     ends

         end
