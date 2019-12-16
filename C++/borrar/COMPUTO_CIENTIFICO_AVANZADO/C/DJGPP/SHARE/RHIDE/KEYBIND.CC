/****************************************************************************

  Keyboard Binding file for the TCEditor class

  created by SET (Salvador Eduardo Tropea salvador@inti.edu.ar)

  Description:
  This file is in fact a C++ source code, to use this file you must use:

  cpp keybind.cpp > keybind.txt

  Where cpp is the C++ preprocessor. You can use any of the comment systems
available in C++ inside the file.
  There are 7 sections in the file, each section is the keyboard translation
under certain situation.
   The sections are:

   NORMAL
   The normal case.

   SHIFT
   Shift plus a key.

   CONTROL
   Control plus a key.

   SHIFT_CONTROL
   Shift plus control plus a key (all at the same time).

   QUICK
   A key after ^Q or wherever you defined as cmbCtrlQ.

   BLOCK
   A key after ^K or wherever you defined as cmbCtrlK.

   BLOCK_SHIFT
   A key plus shift after ^K or wherever you defined as cmbCtrlK.

   In each section you must put the keyboards and the command asociated to it.
   Use the following syntax:

   Scan_Code = Command

   Not all the scan codes are available, for example you can't put:

   RSHIFT = xxxxx

   Look the keybind.h to see the scan codes labels and the available commands.

Important Notes:
I)
  The new assignments are mixed with the original assignments, so if you
want to eliminate an old key combination you *must* assign a cmbNoCommand
to this key, for example is you wish eliminate the BACKSPACE key and use the
^O instead you must:
a) put BACKSPACE = cmbNoCommand in the NORMAL section.
b) put O = cmbBackSpace in the CONTROL section,
  If you only put the b) you'll not eliminate the BACKSPACE key.

II)
  If some thing is wrong in the KEYBIND.TXT file the program will inform the
error, the line number is for the KEYBIND.TXT and not for KEYBIND.CPP (sorry
some day I'll fix this).
  To continue the execution press any key and then enter.

III)
  You can repeat an assignment, the last win:

BACKSPACE = cmbNoCommand
BACKSPACE = cmbBackSpace

  The first is ignored

****************************************************************************/

#include "keybind.h"

[NORMAL]
BACKSPACE = cmbBackSpace
ENTER     = cmbNewLine
HOME      = cmbLineStart
UP        = cmbLineUp
PGUP      = cmbPageUp
LEFT      = cmbCharLeft
RIGHT     = cmbCharRight
END       = cmbLineEnd
DOWN      = cmbLineDown
PGDN      = cmbPageDown
INSERT    = cmbInsMode
DEL       = cmbDelChar
MACRO     = cmbExpandCode

[SHIFT]
BACKSPACE = cmbBackSpace
ENTER     = cmbNewLine
HOME      = cmbLineStart
UP        = cmbLineUp
PGUP      = cmbPageUp
LEFT      = cmbCharLeft
RIGHT     = cmbCharRight
END       = cmbLineEnd
DOWN      = cmbLineDown
PGDN      = cmbPageDown
INSERT    = cmbPaste
DEL       = cmbCut
/*SPACE     = cmbExpandCode    Moved because some people let SHIFT pressed unintentionally
                               while inserting spaces */

[CONTROL]
N1        = cmbGotoMark1
N2        = cmbGotoMark2
N3        = cmbGotoMark3
N4        = cmbGotoMark4
N5        = cmbGotoMark5
N6        = cmbGotoMark6
N7        = cmbGotoMark7
N8        = cmbGotoMark8
N9        = cmbGotoMark9
N0        = cmbGotoMark0
A         = cmbWordLeft
C         = cmbPageDown
D         = cmbCharRight
E         = cmbLineUp
F         = cmbWordRight
G         = cmbDelChar
H         = cmbBackSpace
J         = cmbGotoEditorLine
K         = cmbCtrlK
L         = cmbSearchAgain
M         = cmbNewLine
N         = cmbNewLine
O         = cmbIndentMode
Q         = cmbCtrlQ
R         = cmbPageUp
S         = cmbCharLeft
T         = cmbDelWord
U         = cmbUndo
V         = cmbInsMode
W         = cmbScrollDown
X         = cmbLineDown
Y         = cmbDelLine
Z         = cmbScrollUp
BACKSPACE = cmbDelPrevWord
TAB       = cmbSmartIndent
OPENBRACE = cmbSearchStart
CLOSEBRACE= cmbSearchEnd
PGDN      = cmbTextEnd
PGUP      = cmbTextStart
INSERT    = cmbCopy
DEL       = cmbClear
LEFT      = cmbWordLeft
RIGHT     = cmbWordRight
HOME      = cmbFirstLineInScreen
END       = cmbLastLineInScreen
SPACE     = cmbExpandCode
ENTER     = cmbLoadFileUnderCur

[SHIFT_CONTROL]
N1        = cmbGotoMark1
N2        = cmbGotoMark2
N3        = cmbGotoMark3
N4        = cmbGotoMark4
N5        = cmbGotoMark5
N6        = cmbGotoMark6
N7        = cmbGotoMark7
N8        = cmbGotoMark8
N9        = cmbSearchOpPar
N0        = cmbSearchClPar
A         = cmbWordLeft
C         = cmbPageDown
D         = cmbCharRight
E         = cmbLineUp
F         = cmbWordRight
G         = cmbDelChar
H         = cmbBackSpace
J         = cmbGotoEditorLine
K         = cmbCtrlK
L         = cmbSearchAgain
M         = cmbNewLine
N         = cmbNewLine
P         = cmbLastPosCur
Q         = cmbCtrlQ
R         = cmbReadBlock
S         = cmbCharLeft
T         = cmbDelWord
U         = cmbUndo
V         = cmbInsMode
W         = cmbWriteBlock
X         = cmbLineDown
Y         = cmbDelEnd
Z         = cmbScrollUp
BACKSPACE = cmbDelPrevWord
TAB       = cmbSmartUnIndent
OPENBRACE = cmbSearchOpCor
CLOSEBRACE= cmbSearchClCor
PGDN      = cmbTextEnd
PGUP      = cmbTextStart
INSERT    = cmbReplaceSelect
DEL       = cmbClear
HOME      = cmbFirstLineInScreen
END       = cmbLastLineInScreen
LEFT      = cmbWordLeft
RIGHT     = cmbWordRight

[QUICK]
N1        = cmbGotoMark1
N2        = cmbGotoMark2
N3        = cmbGotoMark3
N4        = cmbGotoMark4
N5        = cmbGotoMark5
N6        = cmbGotoMark6
N7        = cmbGotoMark7
N8        = cmbGotoMark8
N9        = cmbGotoMark9
N0        = cmbGotoMark0
A         = cmbReplace
B         = cmbGoBeginBlock
C         = cmbTextEnd
D         = cmbLineEnd
E         = cmbFirstLineInScreen
F         = cmbFind
H         = cmbDelStart
K         = cmbGoEndBlock
L         = cmbSelLength
R         = cmbTextStart
S         = cmbLineStart
X         = cmbLastLineInScreen
Y         = cmbDelEnd
ESC       = cmbSearchComplement

[BLOCK]
N0        = cmbPutMark0
N1        = cmbPutMark1
N2        = cmbPutMark2
N3        = cmbPutMark3
N4        = cmbPutMark4
N5        = cmbPutMark5
N6        = cmbPutMark6
N7        = cmbPutMark7
N8        = cmbPutMark8
N9        = cmbPutMark9
B         = cmbStartSelect
C         = cmbCopyBlock
H         = cmbHideSelect
I         = cmbIndentBlkOne
K         = cmbEndSelect
L         = cmbMarkLine
M         = cmbToUpper
O         = cmbToLower
R         = cmbReadBlock
T         = cmbMarkWord
U         = cmbUnIndentBlkOne
V         = cmbMoveBlock
W         = cmbWriteBlock
Y         = cmbCut
TAB       = cmbIndentBlk

[BLOCK_SHIFT]
N0        = cmbPutMark0
N1        = cmbPutMark1
N2        = cmbPutMark2
N3        = cmbPutMark3
N4        = cmbPutMark4
N5        = cmbPutMark5
N6        = cmbPutMark6
N7        = cmbPutMark7
N8        = cmbPutMark8
N9        = cmbPutMark9
A         = cmbToggleMoveOnPaste
B         = cmbSelRectStart
C         = cmbSelRectCopy
H         = cmbSelRectHide
I         = cmbIndentBlkOne
K         = cmbSelRectEnd
L         = cmbSelRectDel
M         = cmbSelRectMove
O         = cmbToLower
P         = cmbSelRectPaste
R         = cmbReadBlock
T         = cmbSelRectCut
U         = cmbUnIndentBlkOne
V         = cmbSelRectMove
W         = cmbWriteBlock
Y         = cmbCut
TAB       = cmbUnIndentBlk

