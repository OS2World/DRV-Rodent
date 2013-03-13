#define INCL_WIN
#define INCL_VIO
#define INCL_AVIO
#define INCL_DOSDEVICES

#include <os2.h>

#include <string.h>
#include <stdio.h>

#define wmButton1Click        0
#define wmButton1Dblclk       1
#define wmButton1Down         2
#define wmButton1MotionEnd    3
#define wmButton1MotionStart  4
#define wmButton1Up           5
#define wmButton2Click        6
#define wmButton2Dblclk       7
#define wmButton2Down         8
#define wmButton2MotionEnd    9
#define wmButton2MotionStart 10
#define wmButton2Up          11
#define wmButton3Click       12
#define wmButton3Dblclk      13
#define wmButton3Down        14
#define wmButton3MotionEnd   15
#define wmButton3MotionStart 16
#define wmButton3Up          17
#define wmChord              18
#define wmMouseMove          19

static char *wmName[] =
   {
   " wmButton1Click            ",
   " wmButton1Dblclk           ",
   " wmButton1Down             ",
   " wmButton1MotionEnd        ",
   " wmButton1MotionStart      ",
   " wmButton1Up               ",
   " wmButton2Click            ",
   " wmButton2Dblclk           ",
   " wmButton2Down             ",
   " wmButton2MotionEnd        ",
   " wmButton2MotionStart      ",
   " wmButton2Up               ",
   " wmButton3Click            ",
   " wmButton3Dblclk           ",
   " wmButton3Down             ",
   " wmButton3MotionEnd        ",
   " wmButton3MotionStart      ",
   " wmButton3Up               ",
   " wmChord                   ",
   " wmMouseMove               "
   };

static char *numButts =
   " Number buttons = ?        ";

static char *numMickeys =
   " Number mickeys = ???      ";

static char *dashLine =
   "---------------------------";


MRESULT OurWindowProcedure(
   HWND     hwnd,
   ULONG    msg,
   MPARAM   mp1,
   MPARAM   mp2);

static void DisplayRow(
   long     row,
   HVPS     vioSpace);


static HAB  anchorBlock;

int main(void)
   {
   static ULONG flFrameFlags = FCF_TITLEBAR      | FCF_SYSMENU |
                               FCF_SIZEBORDER    | FCF_MINMAX  |
                               FCF_SHELLPOSITION | FCF_TASKLIST;
   static CHAR szClientClass[] = "Welcome";
   HMQ      messageQueue;
   QMSG     message;
   HWND     frame;
   HWND     client;
   HFILE    mouse;
   ULONG    action;
   ULONG    category;
   ULONG    function;
   PVOID    parmList;
   ULONG    parmLengthMax;
   ULONG    parmLengthInOut;
   USHORT   micksPerCm;
   ULONG    dataLengthMax;
   ULONG    dataLengthInOut;
   APIRET   rc;

   rc = DosOpen("MOUSE$", &mouse, &action, 0, 0, FILE_OPEN, OPEN_SHARE_DENYNONE | OPEN_FLAGS_FAIL_ON_ERROR | OPEN_ACCESS_READWRITE, NULL);
   if (rc)
         sprintf(dashLine, "DosOpen() = %d", rc);
      else
         {
         category        = 0x07;
         function        = 0x61;
         parmList        = NULL;
         parmLengthInOut = 0;
         parmLengthMax   = 0;
         dataLengthInOut = 0;
         dataLengthMax   = 2;
         rc = DosDevIOCtl(mouse, category, function, parmList, parmLengthMax, &parmLengthInOut, &micksPerCm, dataLengthMax, &dataLengthInOut);
         if (rc)
               sprintf(dashLine, "DosDevIOCtl() = %d", rc);
            else
               sprintf(numMickeys+18, "%3hu", micksPerCm);
         DosClose(mouse);
         }
   anchorBlock  = WinInitialize(0);
   messageQueue  = WinCreateMsgQueue(anchorBlock, 0);

   WinRegisterClass(
      anchorBlock,
      szClientClass,
      OurWindowProcedure,
      CS_SIZEREDRAW,
      0);

   frame = WinCreateStdWindow(
      HWND_DESKTOP,
      WS_VISIBLE,
      &flFrameFlags,
      szClientClass,
      NULL,
      0,
      0,
      0,
      &client);

   WinSendMsg(
      frame,
      WM_SETICON,
      (void *)WinQuerySysPointer(
         HWND_DESKTOP,
         SPTR_APPICON,
         FALSE),
      0);

   while (WinGetMsg(anchorBlock, &message, 0, 0, 0))
      WinDispatchMsg(anchorBlock, &message);

   WinDestroyWindow(frame);
   WinDestroyMsgQueue(anchorBlock);
   WinTerminate(anchorBlock);
   return 0;
   }



MRESULT OurWindowProcedure(
   HWND           window,
   ULONG          msg,
   MPARAM         parm1,
   MPARAM         parm2)
   {
   SIZEL          size;
   HDC            deviceContext;
   RECTL          rectangle;
   static HPS     presentationSpace;
   static HVPS    vioSpace;
   long           i;

   switch (msg)
      {
      case WM_CREATE:
         deviceContext = WinOpenWindowDC(window);
         size.cx = size.cy = 0;
         presentationSpace = GpiCreatePS(
            anchorBlock,
            deviceContext,
            &size,
            PU_PELS | GPIF_DEFAULT | GPIT_MICRO | GPIA_ASSOC);
         VioCreatePS(&vioSpace, 24, 31, 0, 1, 0);
         VioAssociate(deviceContext, vioSpace);
         numButts[18] = '0' + WinQuerySysValue(HWND_DESKTOP, SV_CMOUSEBUTTONS);
         VioWrtCharStr(numButts,   27, 0, 0, vioSpace);
         VioWrtCharStr(numMickeys, 27, 1, 0, vioSpace);
         VioWrtCharStr(dashLine,   27, 2, 0, vioSpace);
         return 0;

      case WM_SIZE:
         WinDefAVioWindowProc(window, msg, (ULONG)parm1, (ULONG)parm2);
         return 0;

      case WM_PAINT:
         WinBeginPaint(window, presentationSpace, NULL);
         WinQueryWindowRect(window, &rectangle);
         WinFillRect(presentationSpace, &rectangle, CLR_BLACK);
         VioShowBuf(0, 2 * 24 * 31, vioSpace);
         WinEndPaint(presentationSpace);
         return 0;

      case WM_BUTTON1CLICK:
         DisplayRow(wmButton1Click, vioSpace);
         return 0;

      case WM_BUTTON1DBLCLK:
         DisplayRow(wmButton1Dblclk, vioSpace);
         return 0;

      case WM_BUTTON1DOWN:
         DisplayRow(wmButton1Down, vioSpace);
         return 0;

      case WM_BUTTON1MOTIONEND:
         DisplayRow(wmButton1MotionEnd, vioSpace);
         return 0;

      case WM_BUTTON1MOTIONSTART:
         DisplayRow(wmButton1MotionStart, vioSpace);
         return 0;

      case WM_BUTTON1UP:
         DisplayRow(wmButton1Up, vioSpace);
         return 0;

      case WM_BUTTON2CLICK:
         DisplayRow(wmButton2Click, vioSpace);
         return 0;

      case WM_BUTTON2DBLCLK:
         DisplayRow(wmButton2Dblclk, vioSpace);
         return 0;

      case WM_BUTTON2DOWN:
         DisplayRow(wmButton2Down, vioSpace);
         return 0;

      case WM_BUTTON2MOTIONEND:
         DisplayRow(wmButton2MotionEnd, vioSpace);
         return 0;

      case WM_BUTTON2MOTIONSTART:
         DisplayRow(wmButton2MotionStart, vioSpace);
         return 0;

      case WM_BUTTON2UP:
         DisplayRow(wmButton2Up, vioSpace);
         return 0;

      case WM_BUTTON3CLICK:
         DisplayRow(wmButton3Click, vioSpace);
         return 0;

      case WM_BUTTON3DBLCLK:
         DisplayRow(wmButton3Dblclk, vioSpace);
         return 0;

      case WM_BUTTON3DOWN:
         DisplayRow(wmButton3Down, vioSpace);
         return 0;

      case WM_BUTTON3MOTIONEND:
         DisplayRow(wmButton3MotionEnd, vioSpace);
         return 0;

      case WM_BUTTON3MOTIONSTART:
         DisplayRow(wmButton3MotionStart, vioSpace);
         return 0;

      case WM_BUTTON3UP:
         DisplayRow(wmButton3Up, vioSpace);
         return 0;

      case WM_CHORD:
         DisplayRow(wmChord, vioSpace);
         return 0;

      case WM_MOUSEMOVE:
         DisplayRow(wmMouseMove, vioSpace);
         return 0;

      case WM_DESTROY:
         VioAssociate(0, vioSpace);
         VioDestroyPS(vioSpace);
         GpiDestroyPS(presentationSpace);
         return 0;

      default:
         return WinDefWindowProc(window, msg, parm1, parm2);
      }
   }



static void DisplayRow(
   long           row,
   HVPS           vioSpace)
   {
   static long    lastRow = -1;

   if (lastRow >= 0)
      {
      *wmName[lastRow] = ' ';
      VioWrtCharStr(
         wmName[lastRow],
         27,
         lastRow+3,
         0,
         vioSpace);
      }
   lastRow = row;
   *wmName[lastRow] = '*';
   VioWrtCharStr(
      wmName[lastRow],
      27,
      lastRow+3,
      0,
      vioSpace);
   VioShowBuf(0, 2 * 24 * 31, vioSpace);
   return;
   }
