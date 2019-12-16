/************************** Keyboard Scan Codes *****************************/

/*****************************************************************************

   Not all can be used!!!, for example Shift, Caps Lock, etc.

*****************************************************************************/

//   If you are using a NON-US keyboard you must create a header file for it
// or use one of the allready suplied. For example for german keyboards:
//#define KEYBOARD_HEADER "de.h"

#ifndef KEYBOARD_HEADER

#define ESC         1
#define N1          2
#define N2          3
#define N3          4
#define N4          5
#define N5          6
#define N6          7
#define N7          8
#define N8          9
#define N9          10
#define N0          11
#define MINUS       12
#define EQUALS      13
#define BACKSPACE   14
#define TAB         15
#define Q           16
#define W           17
#define E           18
#define R           19
#define T           20
#define Y           21
#define U           22
#define I           23
#define O           24
#define P           25
#define OPENBRACE   26
#define CLOSEBRACE  27
#define ENTER       28
#define CTRL        29
#define A           30
#define S           31
#define D           32
#define F           33
#define G           34
#define H           35
#define J           36
#define K           37
#define L           38
#define COLON       39
#define QUOTE       40
#define TILDE       41
#define LSHIFT      42
#define BACKSLASH   43
#define Z           44
#define X           45
#define C           46
#define V           47
#define B           48
#define N           49
#define M           50
#define COMMA       51
#define STOP        52
#define SLASH       53
#define RSHIFT      54
#define ASTERISK    55
#define ALT         56
#define SPACE       57
#define CAPSLOCK    58
#define F1          59
#define F2          60
#define F3          61
#define F4          62
#define F5          63
#define F6          64
#define F7          65
#define F8          66
#define F9          67
#define F10         68
#define NUMLOCK     69
#define SCRLOCK     70
#define HOME        71
#define UP          72
#define PGUP        73
#define MINUS_PAD   74
#define LEFT        75
#define PAD_5       76
#define RIGHT       77
#define PLUS_PAD    78
#define END         79
#define DOWN        80
#define PGDN        81
#define INSERT      82
#define DEL         83
#define UNK1        84
#define UNK2        85
#define UNK3        86
#define F11         87
#define F12         88
#define UNK4        89
#define UNK5        90
#define UNK6        91
#define UNK7        92
#define UNK8        93
#define UNK9        94
#define UNK10       95
#define UNK11       96
#define UNK12       97
#define UNK13       98
#define UNK14       99
#define UNK15      100
#define UNK16      101
#define UNK17      102
#define UNK18      103
#define UNK19      104
#define UNK20      105
#define UNK21      106
#define UNK22      107
#define UNK23      108
#define UNK24      109
#define UNK25      110
#define MACRO      111
#define UNK26      112
#define UNK27      113
#define UNK28      114
#define UNK29      115
#define UNK30      116
#define UNK31      117
#define UNK32      118
#define UNK33      119
#define UNK34      120
#define UNK35      121
#define UNK36      122
#define UNK37      123
#define UNK38      124
#define UNK39      125
#define UNK40      126
#define UNK41      127

#else

// Put here the name of your file
#include KEYBOARD_HEADER

#endif

/***************************** Section Names *****************************/
#define NORMAL        1
#define SHIFT         2
#define CONTROL       3
#define SHIFT_CONTROL 4
#define QUICK         5
#define BLOCK         6
#define BLOCK_SHIFT   7

/****************************** Commands *******************************/
// Special commands
// To trigger a block sequence
#define cmbCtrlK              254
// To trigger a Quick sequence
#define cmbCtrlQ              255

// Normal commands
#define cmbNoCommand            0
#define cmbToLower              1
#define cmbSelLength            2
#define cmbFirstLineInScreen    3
#define cmbLastLineInScreen     4
#define cmbIndentBlkOne         5
#define cmbUnIndentBlkOne       6
#define cmbIndentBlk            7
#define cmbUnIndentBlk          8
#define cmbSearchStart          9
#define cmbSearchEnd           10
#define cmbExpandCode          11
#define cmbDelPrevWord         12
#define cmbEndSelect           13
#define cmbSmartUnIndent       14
#define cmbSmartIndent         15
#define cmbReplaceSelect       16
#define cmbGotoEditorLine      17
#define cmbGoBeginBlock        18
#define cmbGoEndBlock          19
#define cmbMarkLine            20
#define cmbMarkWord            21
#define cmbMoveBlock           22
#define cmbScrollDown          23
#define cmbScrollUp            24
#define cmbPutMark0            25
#define cmbPutMark1            26
#define cmbPutMark2            27
#define cmbPutMark3            28
#define cmbPutMark4            29
#define cmbPutMark5            30
#define cmbPutMark6            31
#define cmbPutMark7            32
#define cmbPutMark8            33
#define cmbPutMark9            34
#define cmbGotoMark0           35
#define cmbGotoMark1           36
#define cmbGotoMark2           37
#define cmbGotoMark3           38
#define cmbGotoMark4           39
#define cmbGotoMark5           40
#define cmbGotoMark6           41
#define cmbGotoMark7           42
#define cmbGotoMark8           43
#define cmbGotoMark9           44
#define cmbInsertText          45
#define cmbReadBlock           46
#define cmbWriteBlock          47
#define cmbNewLine             48
#define cmbBackSpace           49
#define cmbCharLeft            50
#define cmbCharRight           51
#define cmbWordLeft            52
#define cmbWordRight           53
#define cmbLineStart           54
#define cmbLineEnd             55
#define cmbLineUp              56
#define cmbLineDown            57
#define cmbPageUp              58
#define cmbPageDown            59
#define cmbTextStart           60
#define cmbTextEnd             61
#define cmbDelChar             62
#define cmbDelWord             63
#define cmbDelStart            64
#define cmbDelEnd              65
#define cmbDelLine             66
#define cmbInsMode             67
#define cmbStartSelect         68
#define cmbHideSelect          69
#define cmbIndentMode          70
#define cmbUpdateTitle         71
#define cmbCut                 72
#define cmbCopy                73
#define cmbPaste               74
#define cmbUndo                75
#define cmbSearchAgain         76
#define cmbClear               77
#define cmbReplace             78
#define cmbFind                79
#define cmbSearchOpPar         80
#define cmbSearchClPar         81
#define cmbSearchOpCor         82
#define cmbSearchClCor         83
#define cmbToUpper             84
#define cmbRedo                85
#define cmbSetLocalOptions     86
#define cmbExpandAllTabs       87
#define cmbCompactBuffer       88
#define cmbRecordMacro         89
#define cmbStopMacro           90
#define cmbPlayMacro           91
#define cmbSetGlobalOptions    92
#define cmbSaveAsUNIX          93
#define cmbCopyBlock           94
#define cmbSelRectStart        95
#define cmbSelRectEnd          96
#define cmbSelRectCopy         97
#define cmbSelRectPaste        98
#define cmbSelRectDel          99
#define cmbSelRectCut         100
#define cmbSelRectMove        101
#define cmbSelRectHide        102
#define cmbLastPosCur         103
#define cmbToggleMoveOnPaste  104
#define cmbJumpToFunction     105
#define cmbProfileEditor      106
#define cmbColorsChanged      107
#define cmbLoadFileUnderCur   108
#define cmbSearchComplement   109

