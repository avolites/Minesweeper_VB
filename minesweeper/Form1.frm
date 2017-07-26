VERSION 5.00
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Minesweeper"
   ClientHeight    =   10740
   ClientLeft      =   1590
   ClientTop       =   735
   ClientWidth     =   18165
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   10740
   ScaleWidth      =   18165
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   19
      Left            =   7200
      Picture         =   "Form1.frx":0ECA
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   848
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   1000
      Left            =   8520
      Top             =   120
   End
   Begin VB.PictureBox Pictureb 
      AutoSize        =   -1  'True
      Height          =   7935
      Left            =   9600
      Picture         =   "Form1.frx":4269
      ScaleHeight     =   7875
      ScaleWidth      =   6825
      TabIndex        =   14
      Top             =   120
      Width           =   6885
      Begin VB.PictureBox befastest 
         Height          =   3015
         Left            =   1680
         ScaleHeight     =   2955
         ScaleWidth      =   3555
         TabIndex        =   849
         Top             =   2640
         Visible         =   0   'False
         Width           =   3615
         Begin VB.CommandButton Command1 
            Caption         =   "OK"
            Height          =   375
            Left            =   960
            TabIndex        =   853
            Top             =   2400
            Width           =   1455
         End
         Begin VB.TextBox betextname 
            Height          =   375
            Left            =   240
            MaxLength       =   25
            TabIndex        =   852
            Text            =   "Anonymous"
            Top             =   1680
            Width           =   3015
         End
         Begin VB.Label Label2 
            Caption         =   "Please enter your name:"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   13.5
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   615
            Left            =   240
            TabIndex        =   851
            Top             =   1080
            Width           =   3255
         End
         Begin VB.Label Label1 
            Caption         =   "You have the fast time for beginner level."
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   13.5
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   1095
            Left            =   240
            TabIndex        =   850
            Top             =   240
            Width           =   3015
         End
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   80
         Left            =   5160
         Picture         =   "Form1.frx":9B65
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   98
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   79
         Left            =   4680
         Picture         =   "Form1.frx":C534
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   97
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   78
         Left            =   4200
         Picture         =   "Form1.frx":EF03
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   96
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   77
         Left            =   3720
         Picture         =   "Form1.frx":118D2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   95
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   76
         Left            =   3240
         Picture         =   "Form1.frx":142A1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   94
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   75
         Left            =   2760
         Picture         =   "Form1.frx":16C70
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   93
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   74
         Left            =   2280
         Picture         =   "Form1.frx":1963F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   92
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   73
         Left            =   1800
         Picture         =   "Form1.frx":1C00E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   91
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   72
         Left            =   1320
         Picture         =   "Form1.frx":1E9DD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   90
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   71
         Left            =   5160
         Picture         =   "Form1.frx":213AC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   89
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   70
         Left            =   4680
         Picture         =   "Form1.frx":23D7B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   88
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   69
         Left            =   4200
         Picture         =   "Form1.frx":2674A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   87
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   68
         Left            =   3720
         Picture         =   "Form1.frx":29119
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   86
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   67
         Left            =   3240
         Picture         =   "Form1.frx":2BAE8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   85
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   66
         Left            =   2760
         Picture         =   "Form1.frx":2E4B7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   84
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   65
         Left            =   2280
         Picture         =   "Form1.frx":30E86
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   83
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   64
         Left            =   1800
         Picture         =   "Form1.frx":33855
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   82
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   63
         Left            =   1320
         Picture         =   "Form1.frx":36224
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   81
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   62
         Left            =   5160
         Picture         =   "Form1.frx":38BF3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   80
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   61
         Left            =   4680
         Picture         =   "Form1.frx":3B5C2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   79
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   60
         Left            =   4200
         Picture         =   "Form1.frx":3DF91
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   78
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   59
         Left            =   3720
         Picture         =   "Form1.frx":40960
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   77
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   58
         Left            =   3240
         Picture         =   "Form1.frx":4332F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   76
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   57
         Left            =   2760
         Picture         =   "Form1.frx":45CFE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   75
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   56
         Left            =   2280
         Picture         =   "Form1.frx":486CD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   74
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   55
         Left            =   1800
         Picture         =   "Form1.frx":4B09C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   73
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   54
         Left            =   1320
         Picture         =   "Form1.frx":4DA6B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   72
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   53
         Left            =   5160
         Picture         =   "Form1.frx":5043A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   71
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   52
         Left            =   4680
         Picture         =   "Form1.frx":52E09
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   70
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   51
         Left            =   4200
         Picture         =   "Form1.frx":557D8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   69
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   50
         Left            =   3720
         Picture         =   "Form1.frx":581A7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   68
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   49
         Left            =   3240
         Picture         =   "Form1.frx":5AB76
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   67
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   48
         Left            =   2760
         Picture         =   "Form1.frx":5D545
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   66
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   47
         Left            =   2280
         Picture         =   "Form1.frx":5FF14
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   65
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   46
         Left            =   1800
         Picture         =   "Form1.frx":628E3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   64
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   45
         Left            =   1320
         Picture         =   "Form1.frx":652B2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   63
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   44
         Left            =   5160
         Picture         =   "Form1.frx":67C81
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   62
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   43
         Left            =   4680
         Picture         =   "Form1.frx":6A650
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   61
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   42
         Left            =   4200
         Picture         =   "Form1.frx":6D01F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   60
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   41
         Left            =   3720
         Picture         =   "Form1.frx":6F9EE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   59
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   40
         Left            =   3240
         Picture         =   "Form1.frx":723BD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   58
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   39
         Left            =   2760
         Picture         =   "Form1.frx":74D8C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   57
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   38
         Left            =   2280
         Picture         =   "Form1.frx":7775B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   56
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   37
         Left            =   1800
         Picture         =   "Form1.frx":7A12A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   55
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   36
         Left            =   1320
         Picture         =   "Form1.frx":7CAF9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   54
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   35
         Left            =   5160
         Picture         =   "Form1.frx":7F4C8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   53
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   34
         Left            =   4680
         Picture         =   "Form1.frx":81E97
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   52
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   33
         Left            =   4200
         Picture         =   "Form1.frx":84866
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   51
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   32
         Left            =   3720
         Picture         =   "Form1.frx":87235
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   50
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   31
         Left            =   3240
         Picture         =   "Form1.frx":89C04
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   49
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   30
         Left            =   2760
         Picture         =   "Form1.frx":8C5D3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   48
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   29
         Left            =   2280
         Picture         =   "Form1.frx":8EFA2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   47
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   28
         Left            =   1800
         Picture         =   "Form1.frx":91971
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   46
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   27
         Left            =   1320
         Picture         =   "Form1.frx":94340
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   45
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   26
         Left            =   5160
         Picture         =   "Form1.frx":96D0F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   44
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   25
         Left            =   4680
         Picture         =   "Form1.frx":996DE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   43
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   24
         Left            =   4200
         Picture         =   "Form1.frx":9C0AD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   42
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   23
         Left            =   3720
         Picture         =   "Form1.frx":9EA7C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   41
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   22
         Left            =   3240
         Picture         =   "Form1.frx":A144B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   40
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   21
         Left            =   2760
         Picture         =   "Form1.frx":A3E1A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   39
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   20
         Left            =   2280
         Picture         =   "Form1.frx":A67E9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   38
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   19
         Left            =   1800
         Picture         =   "Form1.frx":A91B8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   37
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   18
         Left            =   1320
         Picture         =   "Form1.frx":ABB87
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   36
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   17
         Left            =   5160
         Picture         =   "Form1.frx":AE556
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   35
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   16
         Left            =   4680
         Picture         =   "Form1.frx":B0F25
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   34
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   15
         Left            =   4200
         Picture         =   "Form1.frx":B38F4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   33
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   14
         Left            =   3720
         Picture         =   "Form1.frx":B62C3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   32
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   13
         Left            =   3240
         Picture         =   "Form1.frx":B8C92
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   31
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   12
         Left            =   2760
         Picture         =   "Form1.frx":BB661
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   30
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   11
         Left            =   2280
         Picture         =   "Form1.frx":BE030
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   29
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   10
         Left            =   1800
         Picture         =   "Form1.frx":C09FF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   28
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   9
         Left            =   1320
         Picture         =   "Form1.frx":C33CE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   27
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   8
         Left            =   5160
         Picture         =   "Form1.frx":C5D9D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   26
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   7
         Left            =   4680
         Picture         =   "Form1.frx":C876C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   25
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   6
         Left            =   4200
         Picture         =   "Form1.frx":CB13B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   24
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   5
         Left            =   3720
         Picture         =   "Form1.frx":CDB0A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   23
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   4
         Left            =   3240
         Picture         =   "Form1.frx":D04D9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   22
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   3
         Left            =   2760
         Picture         =   "Form1.frx":D2EA8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   21
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   2
         Left            =   2280
         Picture         =   "Form1.frx":D5877
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   20
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   1
         Left            =   1800
         Picture         =   "Form1.frx":D8246
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   19
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picbe 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   0
         Left            =   1320
         Picture         =   "Form1.frx":DAC15
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   18
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox startbe 
         AutoSize        =   -1  'True
         Height          =   750
         Left            =   3120
         Picture         =   "Form1.frx":DD5E4
         ScaleHeight     =   690
         ScaleWidth      =   690
         TabIndex        =   17
         Top             =   480
         Width           =   750
      End
      Begin VB.Label labeltbe 
         Alignment       =   2  'Center
         BackStyle       =   0  'Transparent
         Caption         =   "0"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   18
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000E&
         Height          =   375
         Left            =   4440
         TabIndex        =   845
         Top             =   600
         Width           =   1095
      End
      Begin VB.Label labelmbe 
         Alignment       =   2  'Center
         BackStyle       =   0  'Transparent
         Caption         =   "10"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   18
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000E&
         Height          =   375
         Left            =   1440
         TabIndex        =   844
         Top             =   600
         Width           =   1095
      End
   End
   Begin VB.PictureBox Picture1 
      AutoSize        =   -1  'True
      Height          =   750
      Index           =   18
      Left            =   2160
      Picture         =   "Form1.frx":E1715
      ScaleHeight     =   690
      ScaleWidth      =   690
      TabIndex        =   102
      Top             =   480
      Visible         =   0   'False
      Width           =   750
   End
   Begin VB.PictureBox Picture1 
      AutoSize        =   -1  'True
      Height          =   750
      Index           =   17
      Left            =   1440
      Picture         =   "Form1.frx":E59FB
      ScaleHeight     =   690
      ScaleWidth      =   690
      TabIndex        =   101
      Top             =   480
      Visible         =   0   'False
      Width           =   750
   End
   Begin VB.PictureBox Picture1 
      AutoSize        =   -1  'True
      Height          =   750
      Index           =   16
      Left            =   720
      Picture         =   "Form1.frx":E9D9F
      ScaleHeight     =   690
      ScaleWidth      =   690
      TabIndex        =   100
      Top             =   480
      Visible         =   0   'False
      Width           =   750
   End
   Begin VB.PictureBox Picture1 
      AutoSize        =   -1  'True
      Height          =   750
      Index           =   15
      Left            =   0
      Picture         =   "Form1.frx":EE041
      ScaleHeight     =   690
      ScaleWidth      =   690
      TabIndex        =   99
      Top             =   480
      Visible         =   0   'False
      Width           =   750
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   14
      Left            =   6720
      Picture         =   "Form1.frx":F2172
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   13
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   13
      Left            =   6240
      Picture         =   "Form1.frx":F4B9A
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   12
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   12
      Left            =   5760
      Picture         =   "Form1.frx":F769B
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   11
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   11
      Left            =   5280
      Picture         =   "Form1.frx":FA05A
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   10
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   10
      Left            =   4800
      Picture         =   "Form1.frx":FCB3D
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   9
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   9
      Left            =   4320
      Picture         =   "Form1.frx":FF64A
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   8
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   8
      Left            =   3840
      Picture         =   "Form1.frx":101F7A
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   7
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   7
      Left            =   3360
      Picture         =   "Form1.frx":105159
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   6
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   6
      Left            =   2880
      Picture         =   "Form1.frx":107B76
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   5
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   5
      Left            =   2400
      Picture         =   "Form1.frx":10A5CB
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   4
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   4
      Left            =   1920
      Picture         =   "Form1.frx":10D08C
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   3
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   3
      Left            =   1440
      Picture         =   "Form1.frx":10FAC6
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   2
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   2
      Left            =   960
      Picture         =   "Form1.frx":112576
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   1
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   540
      Index           =   1
      Left            =   480
      Picture         =   "Form1.frx":114FDF
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   0
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picturei 
      AutoSize        =   -1  'True
      Height          =   10560
      Left            =   840
      Picture         =   "Form1.frx":1179D9
      ScaleHeight     =   10500
      ScaleWidth      =   10500
      TabIndex        =   16
      Top             =   3240
      Width           =   10560
      Begin VB.PictureBox infastest 
         Height          =   3015
         Left            =   3360
         ScaleHeight     =   2955
         ScaleWidth      =   3555
         TabIndex        =   854
         Top             =   2160
         Visible         =   0   'False
         Width           =   3615
         Begin VB.TextBox intextname 
            Height          =   375
            Left            =   240
            MaxLength       =   25
            TabIndex        =   856
            Text            =   "Anonymous"
            Top             =   1680
            Width           =   3015
         End
         Begin VB.CommandButton Command2 
            Caption         =   "OK"
            Height          =   375
            Left            =   960
            TabIndex        =   855
            Top             =   2400
            Width           =   1455
         End
         Begin VB.Label Label4 
            Caption         =   "You have the fast time for intermediate level."
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   13.5
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   855
            Left            =   240
            TabIndex        =   858
            Top             =   240
            Width           =   3015
         End
         Begin VB.Label Label3 
            Caption         =   "Please enter your name:"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   13.5
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   615
            Left            =   240
            TabIndex        =   857
            Top             =   1080
            Width           =   3255
         End
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   255
         Left            =   8760
         Picture         =   "Form1.frx":11F36E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   359
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   254
         Left            =   8280
         Picture         =   "Form1.frx":121D3D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   358
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   253
         Left            =   7800
         Picture         =   "Form1.frx":12470C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   357
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   252
         Left            =   7320
         Picture         =   "Form1.frx":1270DB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   356
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   251
         Left            =   6840
         Picture         =   "Form1.frx":129AAA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   355
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   250
         Left            =   6360
         Picture         =   "Form1.frx":12C479
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   354
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   249
         Left            =   5880
         Picture         =   "Form1.frx":12EE48
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   353
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   248
         Left            =   5400
         Picture         =   "Form1.frx":131817
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   352
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   247
         Left            =   4920
         Picture         =   "Form1.frx":1341E6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   351
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   246
         Left            =   4440
         Picture         =   "Form1.frx":136BB5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   350
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   245
         Left            =   3960
         Picture         =   "Form1.frx":139584
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   349
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   244
         Left            =   3480
         Picture         =   "Form1.frx":13BF53
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   348
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   243
         Left            =   3000
         Picture         =   "Form1.frx":13E922
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   347
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   242
         Left            =   2520
         Picture         =   "Form1.frx":1412F1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   346
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   241
         Left            =   2040
         Picture         =   "Form1.frx":143CC0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   345
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   240
         Left            =   1560
         Picture         =   "Form1.frx":14668F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   344
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   239
         Left            =   8760
         Picture         =   "Form1.frx":14905E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   343
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   238
         Left            =   8280
         Picture         =   "Form1.frx":14BA2D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   342
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   237
         Left            =   7800
         Picture         =   "Form1.frx":14E3FC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   341
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   236
         Left            =   7320
         Picture         =   "Form1.frx":150DCB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   340
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   235
         Left            =   6840
         Picture         =   "Form1.frx":15379A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   339
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   234
         Left            =   6360
         Picture         =   "Form1.frx":156169
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   338
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   233
         Left            =   5880
         Picture         =   "Form1.frx":158B38
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   337
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   232
         Left            =   5400
         Picture         =   "Form1.frx":15B507
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   336
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   231
         Left            =   4920
         Picture         =   "Form1.frx":15DED6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   335
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   230
         Left            =   4440
         Picture         =   "Form1.frx":1608A5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   334
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   229
         Left            =   3960
         Picture         =   "Form1.frx":163274
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   333
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   228
         Left            =   3480
         Picture         =   "Form1.frx":165C43
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   332
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   227
         Left            =   3000
         Picture         =   "Form1.frx":168612
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   331
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   226
         Left            =   2520
         Picture         =   "Form1.frx":16AFE1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   330
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   225
         Left            =   2040
         Picture         =   "Form1.frx":16D9B0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   329
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   224
         Left            =   1560
         Picture         =   "Form1.frx":17037F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   328
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   223
         Left            =   8760
         Picture         =   "Form1.frx":172D4E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   327
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   222
         Left            =   8280
         Picture         =   "Form1.frx":17571D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   326
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   221
         Left            =   7800
         Picture         =   "Form1.frx":1780EC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   325
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   220
         Left            =   7320
         Picture         =   "Form1.frx":17AABB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   324
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   219
         Left            =   6840
         Picture         =   "Form1.frx":17D48A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   323
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   218
         Left            =   6360
         Picture         =   "Form1.frx":17FE59
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   322
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   217
         Left            =   5880
         Picture         =   "Form1.frx":182828
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   321
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   216
         Left            =   5400
         Picture         =   "Form1.frx":1851F7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   320
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   215
         Left            =   4920
         Picture         =   "Form1.frx":187BC6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   319
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   214
         Left            =   4440
         Picture         =   "Form1.frx":18A595
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   318
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   213
         Left            =   3960
         Picture         =   "Form1.frx":18CF64
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   317
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   212
         Left            =   3480
         Picture         =   "Form1.frx":18F933
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   316
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   211
         Left            =   3000
         Picture         =   "Form1.frx":192302
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   315
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   210
         Left            =   2520
         Picture         =   "Form1.frx":194CD1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   314
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   209
         Left            =   2040
         Picture         =   "Form1.frx":1976A0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   313
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   208
         Left            =   1560
         Picture         =   "Form1.frx":19A06F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   312
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   207
         Left            =   8760
         Picture         =   "Form1.frx":19CA3E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   311
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   206
         Left            =   8280
         Picture         =   "Form1.frx":19F40D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   310
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   205
         Left            =   7800
         Picture         =   "Form1.frx":1A1DDC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   309
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   204
         Left            =   7320
         Picture         =   "Form1.frx":1A47AB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   308
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   203
         Left            =   6840
         Picture         =   "Form1.frx":1A717A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   307
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   202
         Left            =   6360
         Picture         =   "Form1.frx":1A9B49
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   306
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   201
         Left            =   5880
         Picture         =   "Form1.frx":1AC518
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   305
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   200
         Left            =   5400
         Picture         =   "Form1.frx":1AEEE7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   304
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   199
         Left            =   4920
         Picture         =   "Form1.frx":1B18B6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   303
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   198
         Left            =   4440
         Picture         =   "Form1.frx":1B4285
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   302
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   197
         Left            =   3960
         Picture         =   "Form1.frx":1B6C54
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   301
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   196
         Left            =   3480
         Picture         =   "Form1.frx":1B9623
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   300
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   195
         Left            =   3000
         Picture         =   "Form1.frx":1BBFF2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   299
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   194
         Left            =   2520
         Picture         =   "Form1.frx":1BE9C1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   298
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   193
         Left            =   2040
         Picture         =   "Form1.frx":1C1390
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   297
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   192
         Left            =   1560
         Picture         =   "Form1.frx":1C3D5F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   296
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   191
         Left            =   8760
         Picture         =   "Form1.frx":1C672E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   295
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   190
         Left            =   8280
         Picture         =   "Form1.frx":1C90FD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   294
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   189
         Left            =   7800
         Picture         =   "Form1.frx":1CBACC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   293
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   188
         Left            =   7320
         Picture         =   "Form1.frx":1CE49B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   292
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   187
         Left            =   6840
         Picture         =   "Form1.frx":1D0E6A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   291
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   186
         Left            =   6360
         Picture         =   "Form1.frx":1D3839
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   290
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   185
         Left            =   5880
         Picture         =   "Form1.frx":1D6208
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   289
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   184
         Left            =   5400
         Picture         =   "Form1.frx":1D8BD7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   288
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   183
         Left            =   4920
         Picture         =   "Form1.frx":1DB5A6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   287
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   182
         Left            =   4440
         Picture         =   "Form1.frx":1DDF75
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   286
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   181
         Left            =   3960
         Picture         =   "Form1.frx":1E0944
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   285
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   180
         Left            =   3480
         Picture         =   "Form1.frx":1E3313
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   284
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   179
         Left            =   3000
         Picture         =   "Form1.frx":1E5CE2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   283
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   178
         Left            =   2520
         Picture         =   "Form1.frx":1E86B1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   282
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   177
         Left            =   2040
         Picture         =   "Form1.frx":1EB080
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   281
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   176
         Left            =   1560
         Picture         =   "Form1.frx":1EDA4F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   280
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   175
         Left            =   8760
         Picture         =   "Form1.frx":1F041E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   279
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   174
         Left            =   8280
         Picture         =   "Form1.frx":1F2DED
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   278
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   173
         Left            =   7800
         Picture         =   "Form1.frx":1F57BC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   277
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   172
         Left            =   7320
         Picture         =   "Form1.frx":1F818B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   276
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   171
         Left            =   6840
         Picture         =   "Form1.frx":1FAB5A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   275
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   170
         Left            =   6360
         Picture         =   "Form1.frx":1FD529
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   274
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   169
         Left            =   5880
         Picture         =   "Form1.frx":1FFEF8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   273
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   168
         Left            =   5400
         Picture         =   "Form1.frx":2028C7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   272
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   167
         Left            =   4920
         Picture         =   "Form1.frx":205296
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   271
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   166
         Left            =   4440
         Picture         =   "Form1.frx":207C65
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   270
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   165
         Left            =   3960
         Picture         =   "Form1.frx":20A634
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   269
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   164
         Left            =   3480
         Picture         =   "Form1.frx":20D003
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   268
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   163
         Left            =   3000
         Picture         =   "Form1.frx":20F9D2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   267
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   162
         Left            =   2520
         Picture         =   "Form1.frx":2123A1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   266
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   161
         Left            =   2040
         Picture         =   "Form1.frx":214D70
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   265
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   160
         Left            =   1560
         Picture         =   "Form1.frx":21773F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   264
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   159
         Left            =   8760
         Picture         =   "Form1.frx":21A10E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   263
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   158
         Left            =   8280
         Picture         =   "Form1.frx":21CADD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   262
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   157
         Left            =   7800
         Picture         =   "Form1.frx":21F4AC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   261
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   156
         Left            =   7320
         Picture         =   "Form1.frx":221E7B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   260
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   155
         Left            =   6840
         Picture         =   "Form1.frx":22484A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   259
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   154
         Left            =   6360
         Picture         =   "Form1.frx":227219
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   258
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   153
         Left            =   5880
         Picture         =   "Form1.frx":229BE8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   257
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   152
         Left            =   5400
         Picture         =   "Form1.frx":22C5B7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   256
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   151
         Left            =   4920
         Picture         =   "Form1.frx":22EF86
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   255
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   150
         Left            =   4440
         Picture         =   "Form1.frx":231955
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   254
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   149
         Left            =   3960
         Picture         =   "Form1.frx":234324
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   253
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   148
         Left            =   3480
         Picture         =   "Form1.frx":236CF3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   252
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   147
         Left            =   3000
         Picture         =   "Form1.frx":2396C2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   251
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   146
         Left            =   2520
         Picture         =   "Form1.frx":23C091
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   250
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   145
         Left            =   2040
         Picture         =   "Form1.frx":23EA60
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   249
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   144
         Left            =   1560
         Picture         =   "Form1.frx":24142F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   248
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   143
         Left            =   8760
         Picture         =   "Form1.frx":243DFE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   247
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   142
         Left            =   8280
         Picture         =   "Form1.frx":2467CD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   246
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   141
         Left            =   7800
         Picture         =   "Form1.frx":24919C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   245
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   140
         Left            =   7320
         Picture         =   "Form1.frx":24BB6B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   244
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   139
         Left            =   6840
         Picture         =   "Form1.frx":24E53A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   243
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   138
         Left            =   6360
         Picture         =   "Form1.frx":250F09
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   242
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   137
         Left            =   5880
         Picture         =   "Form1.frx":2538D8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   241
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   136
         Left            =   5400
         Picture         =   "Form1.frx":2562A7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   240
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   135
         Left            =   4920
         Picture         =   "Form1.frx":258C76
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   239
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   134
         Left            =   4440
         Picture         =   "Form1.frx":25B645
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   238
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   133
         Left            =   3960
         Picture         =   "Form1.frx":25E014
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   237
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   132
         Left            =   3480
         Picture         =   "Form1.frx":2609E3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   236
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   131
         Left            =   3000
         Picture         =   "Form1.frx":2633B2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   235
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   130
         Left            =   2520
         Picture         =   "Form1.frx":265D81
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   234
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   129
         Left            =   2040
         Picture         =   "Form1.frx":268750
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   233
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   128
         Left            =   1560
         Picture         =   "Form1.frx":26B11F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   232
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   127
         Left            =   8760
         Picture         =   "Form1.frx":26DAEE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   231
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   126
         Left            =   8280
         Picture         =   "Form1.frx":2704BD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   230
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   125
         Left            =   7800
         Picture         =   "Form1.frx":272E8C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   229
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   124
         Left            =   7320
         Picture         =   "Form1.frx":27585B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   228
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   123
         Left            =   6840
         Picture         =   "Form1.frx":27822A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   227
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   122
         Left            =   6360
         Picture         =   "Form1.frx":27ABF9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   226
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   121
         Left            =   5880
         Picture         =   "Form1.frx":27D5C8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   225
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   120
         Left            =   5400
         Picture         =   "Form1.frx":27FF97
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   224
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   119
         Left            =   4920
         Picture         =   "Form1.frx":282966
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   223
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   118
         Left            =   4440
         Picture         =   "Form1.frx":285335
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   222
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   117
         Left            =   3960
         Picture         =   "Form1.frx":287D04
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   221
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   116
         Left            =   3480
         Picture         =   "Form1.frx":28A6D3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   220
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   115
         Left            =   3000
         Picture         =   "Form1.frx":28D0A2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   219
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   114
         Left            =   2520
         Picture         =   "Form1.frx":28FA71
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   218
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   113
         Left            =   2040
         Picture         =   "Form1.frx":292440
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   217
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   112
         Left            =   1560
         Picture         =   "Form1.frx":294E0F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   216
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   111
         Left            =   8760
         Picture         =   "Form1.frx":2977DE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   215
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   110
         Left            =   8280
         Picture         =   "Form1.frx":29A1AD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   214
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   109
         Left            =   7800
         Picture         =   "Form1.frx":29CB7C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   213
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   108
         Left            =   7320
         Picture         =   "Form1.frx":29F54B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   212
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   107
         Left            =   6840
         Picture         =   "Form1.frx":2A1F1A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   211
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   106
         Left            =   6360
         Picture         =   "Form1.frx":2A48E9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   210
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   105
         Left            =   5880
         Picture         =   "Form1.frx":2A72B8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   209
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   104
         Left            =   5400
         Picture         =   "Form1.frx":2A9C87
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   208
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   103
         Left            =   4920
         Picture         =   "Form1.frx":2AC656
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   207
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   102
         Left            =   4440
         Picture         =   "Form1.frx":2AF025
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   206
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   101
         Left            =   3960
         Picture         =   "Form1.frx":2B19F4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   205
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   100
         Left            =   3480
         Picture         =   "Form1.frx":2B43C3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   204
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   99
         Left            =   3000
         Picture         =   "Form1.frx":2B6D92
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   203
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   98
         Left            =   2520
         Picture         =   "Form1.frx":2B9761
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   202
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   97
         Left            =   2040
         Picture         =   "Form1.frx":2BC130
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   201
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   96
         Left            =   1560
         Picture         =   "Form1.frx":2BEAFF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   200
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   95
         Left            =   8760
         Picture         =   "Form1.frx":2C14CE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   199
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   94
         Left            =   8280
         Picture         =   "Form1.frx":2C3E9D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   198
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   93
         Left            =   7800
         Picture         =   "Form1.frx":2C686C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   197
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   92
         Left            =   7320
         Picture         =   "Form1.frx":2C923B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   196
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   91
         Left            =   6840
         Picture         =   "Form1.frx":2CBC0A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   195
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   90
         Left            =   6360
         Picture         =   "Form1.frx":2CE5D9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   194
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   89
         Left            =   5880
         Picture         =   "Form1.frx":2D0FA8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   193
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   88
         Left            =   5400
         Picture         =   "Form1.frx":2D3977
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   192
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   87
         Left            =   4920
         Picture         =   "Form1.frx":2D6346
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   191
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   86
         Left            =   4440
         Picture         =   "Form1.frx":2D8D15
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   190
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   85
         Left            =   3960
         Picture         =   "Form1.frx":2DB6E4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   189
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   84
         Left            =   3480
         Picture         =   "Form1.frx":2DE0B3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   188
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   83
         Left            =   3000
         Picture         =   "Form1.frx":2E0A82
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   187
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   82
         Left            =   2520
         Picture         =   "Form1.frx":2E3451
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   186
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   81
         Left            =   2040
         Picture         =   "Form1.frx":2E5E20
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   185
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   80
         Left            =   1560
         Picture         =   "Form1.frx":2E87EF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   184
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   79
         Left            =   8760
         Picture         =   "Form1.frx":2EB1BE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   183
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   78
         Left            =   8280
         Picture         =   "Form1.frx":2EDB8D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   182
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   77
         Left            =   7800
         Picture         =   "Form1.frx":2F055C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   181
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   76
         Left            =   7320
         Picture         =   "Form1.frx":2F2F2B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   180
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   75
         Left            =   6840
         Picture         =   "Form1.frx":2F58FA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   179
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   74
         Left            =   6360
         Picture         =   "Form1.frx":2F82C9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   178
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   73
         Left            =   5880
         Picture         =   "Form1.frx":2FAC98
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   177
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   72
         Left            =   5400
         Picture         =   "Form1.frx":2FD667
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   176
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   71
         Left            =   4920
         Picture         =   "Form1.frx":300036
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   175
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   70
         Left            =   4440
         Picture         =   "Form1.frx":302A05
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   174
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   69
         Left            =   3960
         Picture         =   "Form1.frx":3053D4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   173
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   68
         Left            =   3480
         Picture         =   "Form1.frx":307DA3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   172
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   67
         Left            =   3000
         Picture         =   "Form1.frx":30A772
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   171
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   66
         Left            =   2520
         Picture         =   "Form1.frx":30D141
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   170
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   65
         Left            =   2040
         Picture         =   "Form1.frx":30FB10
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   169
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   64
         Left            =   1560
         Picture         =   "Form1.frx":3124DF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   168
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   63
         Left            =   8760
         Picture         =   "Form1.frx":314EAE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   167
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   62
         Left            =   8280
         Picture         =   "Form1.frx":31787D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   166
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   61
         Left            =   7800
         Picture         =   "Form1.frx":31A24C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   165
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   60
         Left            =   7320
         Picture         =   "Form1.frx":31CC1B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   164
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   59
         Left            =   6840
         Picture         =   "Form1.frx":31F5EA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   163
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   58
         Left            =   6360
         Picture         =   "Form1.frx":321FB9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   162
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   57
         Left            =   5880
         Picture         =   "Form1.frx":324988
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   161
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   56
         Left            =   5400
         Picture         =   "Form1.frx":327357
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   160
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   55
         Left            =   4920
         Picture         =   "Form1.frx":329D26
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   159
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   54
         Left            =   4440
         Picture         =   "Form1.frx":32C6F5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   158
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   53
         Left            =   3960
         Picture         =   "Form1.frx":32F0C4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   157
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   52
         Left            =   3480
         Picture         =   "Form1.frx":331A93
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   156
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   51
         Left            =   3000
         Picture         =   "Form1.frx":334462
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   155
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   50
         Left            =   2520
         Picture         =   "Form1.frx":336E31
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   154
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   49
         Left            =   2040
         Picture         =   "Form1.frx":339800
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   153
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   48
         Left            =   1560
         Picture         =   "Form1.frx":33C1CF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   152
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   47
         Left            =   8760
         Picture         =   "Form1.frx":33EB9E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   151
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   46
         Left            =   8280
         Picture         =   "Form1.frx":34156D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   150
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   45
         Left            =   7800
         Picture         =   "Form1.frx":343F3C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   149
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   44
         Left            =   7320
         Picture         =   "Form1.frx":34690B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   148
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   43
         Left            =   6840
         Picture         =   "Form1.frx":3492DA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   147
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   42
         Left            =   6360
         Picture         =   "Form1.frx":34BCA9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   146
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   41
         Left            =   5880
         Picture         =   "Form1.frx":34E678
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   145
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   40
         Left            =   5400
         Picture         =   "Form1.frx":351047
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   144
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   39
         Left            =   4920
         Picture         =   "Form1.frx":353A16
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   143
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   38
         Left            =   4440
         Picture         =   "Form1.frx":3563E5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   142
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   37
         Left            =   3960
         Picture         =   "Form1.frx":358DB4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   141
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   36
         Left            =   3480
         Picture         =   "Form1.frx":35B783
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   140
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   35
         Left            =   3000
         Picture         =   "Form1.frx":35E152
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   139
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   34
         Left            =   2520
         Picture         =   "Form1.frx":360B21
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   138
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   33
         Left            =   2040
         Picture         =   "Form1.frx":3634F0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   137
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   32
         Left            =   1560
         Picture         =   "Form1.frx":365EBF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   136
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   31
         Left            =   8760
         Picture         =   "Form1.frx":36888E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   135
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   30
         Left            =   8280
         Picture         =   "Form1.frx":36B25D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   134
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   29
         Left            =   7800
         Picture         =   "Form1.frx":36DC2C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   133
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   28
         Left            =   7320
         Picture         =   "Form1.frx":3705FB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   132
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   27
         Left            =   6840
         Picture         =   "Form1.frx":372FCA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   131
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   26
         Left            =   6360
         Picture         =   "Form1.frx":375999
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   130
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   25
         Left            =   5880
         Picture         =   "Form1.frx":378368
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   129
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   24
         Left            =   5400
         Picture         =   "Form1.frx":37AD37
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   128
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   23
         Left            =   4920
         Picture         =   "Form1.frx":37D706
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   127
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   22
         Left            =   4440
         Picture         =   "Form1.frx":3800D5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   126
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   21
         Left            =   3960
         Picture         =   "Form1.frx":382AA4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   125
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   20
         Left            =   3480
         Picture         =   "Form1.frx":385473
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   124
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   19
         Left            =   3000
         Picture         =   "Form1.frx":387E42
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   123
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   18
         Left            =   2520
         Picture         =   "Form1.frx":38A811
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   122
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   17
         Left            =   2040
         Picture         =   "Form1.frx":38D1E0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   121
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   16
         Left            =   1560
         Picture         =   "Form1.frx":38FBAF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   120
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   15
         Left            =   8760
         Picture         =   "Form1.frx":39257E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   119
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   14
         Left            =   8280
         Picture         =   "Form1.frx":394F4D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   118
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   13
         Left            =   7800
         Picture         =   "Form1.frx":39791C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   117
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   12
         Left            =   7320
         Picture         =   "Form1.frx":39A2EB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   116
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   11
         Left            =   6840
         Picture         =   "Form1.frx":39CCBA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   115
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   10
         Left            =   6360
         Picture         =   "Form1.frx":39F689
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   114
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   9
         Left            =   5880
         Picture         =   "Form1.frx":3A2058
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   113
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   8
         Left            =   5400
         Picture         =   "Form1.frx":3A4A27
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   112
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   7
         Left            =   4920
         Picture         =   "Form1.frx":3A73F6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   111
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   6
         Left            =   4440
         Picture         =   "Form1.frx":3A9DC5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   110
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   5
         Left            =   3960
         Picture         =   "Form1.frx":3AC794
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   109
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   4
         Left            =   3480
         Picture         =   "Form1.frx":3AF163
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   108
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   3
         Left            =   3000
         Picture         =   "Form1.frx":3B1B32
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   107
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   2
         Left            =   2520
         Picture         =   "Form1.frx":3B4501
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   106
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   1
         Left            =   2040
         Picture         =   "Form1.frx":3B6ED0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   105
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picin 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   0
         Left            =   1560
         Picture         =   "Form1.frx":3B989F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   104
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox startin 
         AutoSize        =   -1  'True
         Height          =   750
         Left            =   4680
         Picture         =   "Form1.frx":3BC26E
         ScaleHeight     =   690
         ScaleWidth      =   690
         TabIndex        =   103
         Top             =   480
         Width           =   750
      End
      Begin VB.Label Labeltin 
         Alignment       =   2  'Center
         BackStyle       =   0  'Transparent
         Caption         =   "0"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   18
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000E&
         Height          =   375
         Left            =   7800
         TabIndex        =   847
         Top             =   600
         Width           =   1095
      End
      Begin VB.Label Labelmin 
         Alignment       =   2  'Center
         BackStyle       =   0  'Transparent
         Caption         =   "40"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   18
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000E&
         Height          =   375
         Left            =   1680
         TabIndex        =   846
         Top             =   600
         Width           =   1095
      End
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H80000008&
      BorderStyle     =   0  'None
      Height          =   545
      Index           =   0
      Left            =   0
      Picture         =   "Form1.frx":3C039F
      ScaleHeight     =   540
      ScaleWidth      =   540
      TabIndex        =   393
      Top             =   0
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox Picturee 
      AutoSize        =   -1  'True
      Height          =   10560
      Left            =   -600
      Picture         =   "Form1.frx":3C2D6E
      ScaleHeight     =   10500
      ScaleWidth      =   17850
      TabIndex        =   15
      Top             =   -120
      Width           =   17910
      Begin VB.PictureBox exfastest 
         Height          =   3015
         Left            =   6720
         ScaleHeight     =   2955
         ScaleWidth      =   3555
         TabIndex        =   859
         Top             =   2160
         Visible         =   0   'False
         Width           =   3615
         Begin VB.CommandButton Command3 
            Caption         =   "OK"
            Height          =   375
            Left            =   960
            TabIndex        =   861
            Top             =   2400
            Width           =   1455
         End
         Begin VB.TextBox extextname 
            Height          =   375
            Left            =   240
            MaxLength       =   25
            TabIndex        =   860
            Text            =   "Anonymous"
            Top             =   1680
            Width           =   3015
         End
         Begin VB.Label Label6 
            Caption         =   "Please enter your name:"
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   13.5
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   615
            Left            =   240
            TabIndex        =   863
            Top             =   1080
            Width           =   3255
         End
         Begin VB.Label Label5 
            Caption         =   "You have the fast time for expert level."
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   13.5
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   855
            Left            =   240
            TabIndex        =   862
            Top             =   240
            Width           =   3015
         End
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   479
         Left            =   15600
         Picture         =   "Form1.frx":3CA9B4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   841
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   478
         Left            =   15120
         Picture         =   "Form1.frx":3CD383
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   840
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   477
         Left            =   14640
         Picture         =   "Form1.frx":3CFD52
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   839
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   476
         Left            =   14160
         Picture         =   "Form1.frx":3D2721
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   838
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   475
         Left            =   13680
         Picture         =   "Form1.frx":3D50F0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   837
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   474
         Left            =   13200
         Picture         =   "Form1.frx":3D7ABF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   836
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   473
         Left            =   12720
         Picture         =   "Form1.frx":3DA48E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   835
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   472
         Left            =   12240
         Picture         =   "Form1.frx":3DCE5D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   834
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   471
         Left            =   11760
         Picture         =   "Form1.frx":3DF82C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   833
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   470
         Left            =   11280
         Picture         =   "Form1.frx":3E21FB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   832
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   469
         Left            =   10800
         Picture         =   "Form1.frx":3E4BCA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   831
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   468
         Left            =   10320
         Picture         =   "Form1.frx":3E7599
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   830
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   467
         Left            =   9840
         Picture         =   "Form1.frx":3E9F68
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   829
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   466
         Left            =   9360
         Picture         =   "Form1.frx":3EC937
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   828
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   465
         Left            =   8880
         Picture         =   "Form1.frx":3EF306
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   827
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   464
         Left            =   8400
         Picture         =   "Form1.frx":3F1CD5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   826
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   463
         Left            =   7920
         Picture         =   "Form1.frx":3F46A4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   825
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   462
         Left            =   7440
         Picture         =   "Form1.frx":3F7073
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   824
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   461
         Left            =   6960
         Picture         =   "Form1.frx":3F9A42
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   823
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   460
         Left            =   6480
         Picture         =   "Form1.frx":3FC411
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   822
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   459
         Left            =   6000
         Picture         =   "Form1.frx":3FEDE0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   821
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   458
         Left            =   5520
         Picture         =   "Form1.frx":4017AF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   820
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   457
         Left            =   5040
         Picture         =   "Form1.frx":40417E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   819
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   456
         Left            =   4560
         Picture         =   "Form1.frx":406B4D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   818
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   455
         Left            =   4080
         Picture         =   "Form1.frx":40951C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   817
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   454
         Left            =   3600
         Picture         =   "Form1.frx":40BEEB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   816
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   453
         Left            =   3120
         Picture         =   "Form1.frx":40E8BA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   815
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   452
         Left            =   2640
         Picture         =   "Form1.frx":411289
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   814
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   451
         Left            =   2160
         Picture         =   "Form1.frx":413C58
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   813
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   450
         Left            =   1680
         Picture         =   "Form1.frx":416627
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   812
         Top             =   8880
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   449
         Left            =   15600
         Picture         =   "Form1.frx":418FF6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   811
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   448
         Left            =   15120
         Picture         =   "Form1.frx":41B9C5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   810
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   447
         Left            =   14640
         Picture         =   "Form1.frx":41E394
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   809
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   446
         Left            =   14160
         Picture         =   "Form1.frx":420D63
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   808
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   445
         Left            =   13680
         Picture         =   "Form1.frx":423732
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   807
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   444
         Left            =   13200
         Picture         =   "Form1.frx":426101
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   806
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   443
         Left            =   12720
         Picture         =   "Form1.frx":428AD0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   805
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   442
         Left            =   12240
         Picture         =   "Form1.frx":42B49F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   804
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   441
         Left            =   11760
         Picture         =   "Form1.frx":42DE6E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   803
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   440
         Left            =   11280
         Picture         =   "Form1.frx":43083D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   802
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   439
         Left            =   10800
         Picture         =   "Form1.frx":43320C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   801
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   438
         Left            =   10320
         Picture         =   "Form1.frx":435BDB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   800
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   437
         Left            =   9840
         Picture         =   "Form1.frx":4385AA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   799
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   436
         Left            =   9360
         Picture         =   "Form1.frx":43AF79
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   798
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   435
         Left            =   8880
         Picture         =   "Form1.frx":43D948
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   797
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   434
         Left            =   8400
         Picture         =   "Form1.frx":440317
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   796
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   433
         Left            =   7920
         Picture         =   "Form1.frx":442CE6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   795
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   432
         Left            =   7440
         Picture         =   "Form1.frx":4456B5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   794
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   431
         Left            =   6960
         Picture         =   "Form1.frx":448084
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   793
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   430
         Left            =   6480
         Picture         =   "Form1.frx":44AA53
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   792
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   429
         Left            =   6000
         Picture         =   "Form1.frx":44D422
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   791
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   428
         Left            =   5520
         Picture         =   "Form1.frx":44FDF1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   790
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   427
         Left            =   5040
         Picture         =   "Form1.frx":4527C0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   789
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   426
         Left            =   4560
         Picture         =   "Form1.frx":45518F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   788
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   425
         Left            =   4080
         Picture         =   "Form1.frx":457B5E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   787
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   424
         Left            =   3600
         Picture         =   "Form1.frx":45A52D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   786
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   423
         Left            =   3120
         Picture         =   "Form1.frx":45CEFC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   785
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   422
         Left            =   2640
         Picture         =   "Form1.frx":45F8CB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   784
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   421
         Left            =   2160
         Picture         =   "Form1.frx":46229A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   783
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   420
         Left            =   1680
         Picture         =   "Form1.frx":464C69
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   782
         Top             =   8400
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   419
         Left            =   15600
         Picture         =   "Form1.frx":467638
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   781
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   418
         Left            =   15120
         Picture         =   "Form1.frx":46A007
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   780
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   417
         Left            =   14640
         Picture         =   "Form1.frx":46C9D6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   779
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   416
         Left            =   14160
         Picture         =   "Form1.frx":46F3A5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   778
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   415
         Left            =   13680
         Picture         =   "Form1.frx":471D74
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   777
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   414
         Left            =   13200
         Picture         =   "Form1.frx":474743
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   776
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   413
         Left            =   12720
         Picture         =   "Form1.frx":477112
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   775
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   412
         Left            =   12240
         Picture         =   "Form1.frx":479AE1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   774
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   411
         Left            =   11760
         Picture         =   "Form1.frx":47C4B0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   773
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   410
         Left            =   11280
         Picture         =   "Form1.frx":47EE7F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   772
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   409
         Left            =   10800
         Picture         =   "Form1.frx":48184E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   771
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   408
         Left            =   10320
         Picture         =   "Form1.frx":48421D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   770
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   407
         Left            =   9840
         Picture         =   "Form1.frx":486BEC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   769
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   406
         Left            =   9360
         Picture         =   "Form1.frx":4895BB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   768
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   405
         Left            =   8880
         Picture         =   "Form1.frx":48BF8A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   767
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   404
         Left            =   8400
         Picture         =   "Form1.frx":48E959
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   766
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   403
         Left            =   7920
         Picture         =   "Form1.frx":491328
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   765
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   402
         Left            =   7440
         Picture         =   "Form1.frx":493CF7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   764
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   401
         Left            =   6960
         Picture         =   "Form1.frx":4966C6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   763
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   400
         Left            =   6480
         Picture         =   "Form1.frx":499095
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   762
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   399
         Left            =   6000
         Picture         =   "Form1.frx":49BA64
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   761
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   398
         Left            =   5520
         Picture         =   "Form1.frx":49E433
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   760
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   397
         Left            =   5040
         Picture         =   "Form1.frx":4A0E02
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   759
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   396
         Left            =   4560
         Picture         =   "Form1.frx":4A37D1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   758
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   395
         Left            =   4080
         Picture         =   "Form1.frx":4A61A0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   757
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   394
         Left            =   3600
         Picture         =   "Form1.frx":4A8B6F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   756
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   393
         Left            =   3120
         Picture         =   "Form1.frx":4AB53E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   755
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   392
         Left            =   2640
         Picture         =   "Form1.frx":4ADF0D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   754
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   391
         Left            =   2160
         Picture         =   "Form1.frx":4B08DC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   753
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   390
         Left            =   1680
         Picture         =   "Form1.frx":4B32AB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   752
         Top             =   7920
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   389
         Left            =   15600
         Picture         =   "Form1.frx":4B5C7A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   751
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   388
         Left            =   15120
         Picture         =   "Form1.frx":4B8649
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   750
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   387
         Left            =   14640
         Picture         =   "Form1.frx":4BB018
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   749
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   386
         Left            =   14160
         Picture         =   "Form1.frx":4BD9E7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   748
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   385
         Left            =   13680
         Picture         =   "Form1.frx":4C03B6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   747
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   384
         Left            =   13200
         Picture         =   "Form1.frx":4C2D85
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   746
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   383
         Left            =   12720
         Picture         =   "Form1.frx":4C5754
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   745
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   382
         Left            =   12240
         Picture         =   "Form1.frx":4C8123
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   744
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   381
         Left            =   11760
         Picture         =   "Form1.frx":4CAAF2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   743
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   380
         Left            =   11280
         Picture         =   "Form1.frx":4CD4C1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   742
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   379
         Left            =   10800
         Picture         =   "Form1.frx":4CFE90
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   741
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   378
         Left            =   10320
         Picture         =   "Form1.frx":4D285F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   740
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   377
         Left            =   9840
         Picture         =   "Form1.frx":4D522E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   739
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   376
         Left            =   9360
         Picture         =   "Form1.frx":4D7BFD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   738
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   375
         Left            =   8880
         Picture         =   "Form1.frx":4DA5CC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   737
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   374
         Left            =   8400
         Picture         =   "Form1.frx":4DCF9B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   736
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   373
         Left            =   7920
         Picture         =   "Form1.frx":4DF96A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   735
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   372
         Left            =   7440
         Picture         =   "Form1.frx":4E2339
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   734
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   371
         Left            =   6960
         Picture         =   "Form1.frx":4E4D08
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   733
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   370
         Left            =   6480
         Picture         =   "Form1.frx":4E76D7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   732
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   369
         Left            =   6000
         Picture         =   "Form1.frx":4EA0A6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   731
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   368
         Left            =   5520
         Picture         =   "Form1.frx":4ECA75
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   730
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   367
         Left            =   5040
         Picture         =   "Form1.frx":4EF444
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   729
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   366
         Left            =   4560
         Picture         =   "Form1.frx":4F1E13
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   728
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   365
         Left            =   4080
         Picture         =   "Form1.frx":4F47E2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   727
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   364
         Left            =   3600
         Picture         =   "Form1.frx":4F71B1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   726
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   363
         Left            =   3120
         Picture         =   "Form1.frx":4F9B80
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   725
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   362
         Left            =   2640
         Picture         =   "Form1.frx":4FC54F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   724
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   361
         Left            =   2160
         Picture         =   "Form1.frx":4FEF1E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   723
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   360
         Left            =   1680
         Picture         =   "Form1.frx":5018ED
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   722
         Top             =   7440
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   359
         Left            =   15600
         Picture         =   "Form1.frx":5042BC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   721
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   358
         Left            =   15120
         Picture         =   "Form1.frx":506C8B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   720
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   357
         Left            =   14640
         Picture         =   "Form1.frx":50965A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   719
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   356
         Left            =   14160
         Picture         =   "Form1.frx":50C029
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   718
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   355
         Left            =   13680
         Picture         =   "Form1.frx":50E9F8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   717
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   354
         Left            =   13200
         Picture         =   "Form1.frx":5113C7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   716
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   353
         Left            =   12720
         Picture         =   "Form1.frx":513D96
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   715
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   352
         Left            =   12240
         Picture         =   "Form1.frx":516765
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   714
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   351
         Left            =   11760
         Picture         =   "Form1.frx":519134
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   713
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   350
         Left            =   11280
         Picture         =   "Form1.frx":51BB03
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   712
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   349
         Left            =   10800
         Picture         =   "Form1.frx":51E4D2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   711
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   348
         Left            =   10320
         Picture         =   "Form1.frx":520EA1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   710
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   347
         Left            =   9840
         Picture         =   "Form1.frx":523870
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   709
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   346
         Left            =   9360
         Picture         =   "Form1.frx":52623F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   708
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   345
         Left            =   8880
         Picture         =   "Form1.frx":528C0E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   707
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   344
         Left            =   8400
         Picture         =   "Form1.frx":52B5DD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   706
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   343
         Left            =   7920
         Picture         =   "Form1.frx":52DFAC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   705
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   342
         Left            =   7440
         Picture         =   "Form1.frx":53097B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   704
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   341
         Left            =   6960
         Picture         =   "Form1.frx":53334A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   703
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   340
         Left            =   6480
         Picture         =   "Form1.frx":535D19
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   702
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   339
         Left            =   6000
         Picture         =   "Form1.frx":5386E8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   701
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   338
         Left            =   5520
         Picture         =   "Form1.frx":53B0B7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   700
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   337
         Left            =   5040
         Picture         =   "Form1.frx":53DA86
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   699
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   336
         Left            =   4560
         Picture         =   "Form1.frx":540455
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   698
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   335
         Left            =   4080
         Picture         =   "Form1.frx":542E24
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   697
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   334
         Left            =   3600
         Picture         =   "Form1.frx":5457F3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   696
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   333
         Left            =   3120
         Picture         =   "Form1.frx":5481C2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   695
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   332
         Left            =   2640
         Picture         =   "Form1.frx":54AB91
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   694
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   331
         Left            =   2160
         Picture         =   "Form1.frx":54D560
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   693
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   330
         Left            =   1680
         Picture         =   "Form1.frx":54FF2F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   692
         Top             =   6960
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   329
         Left            =   15600
         Picture         =   "Form1.frx":5528FE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   691
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   328
         Left            =   15120
         Picture         =   "Form1.frx":5552CD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   690
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   327
         Left            =   14640
         Picture         =   "Form1.frx":557C9C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   689
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   326
         Left            =   14160
         Picture         =   "Form1.frx":55A66B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   688
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   325
         Left            =   13680
         Picture         =   "Form1.frx":55D03A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   687
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   324
         Left            =   13200
         Picture         =   "Form1.frx":55FA09
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   686
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   323
         Left            =   12720
         Picture         =   "Form1.frx":5623D8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   685
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   322
         Left            =   12240
         Picture         =   "Form1.frx":564DA7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   684
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   321
         Left            =   11760
         Picture         =   "Form1.frx":567776
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   683
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   320
         Left            =   11280
         Picture         =   "Form1.frx":56A145
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   682
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   319
         Left            =   10800
         Picture         =   "Form1.frx":56CB14
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   681
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   318
         Left            =   10320
         Picture         =   "Form1.frx":56F4E3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   680
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   317
         Left            =   9840
         Picture         =   "Form1.frx":571EB2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   679
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   316
         Left            =   9360
         Picture         =   "Form1.frx":574881
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   678
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   315
         Left            =   8880
         Picture         =   "Form1.frx":577250
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   677
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   314
         Left            =   8400
         Picture         =   "Form1.frx":579C1F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   676
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   313
         Left            =   7920
         Picture         =   "Form1.frx":57C5EE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   675
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   312
         Left            =   7440
         Picture         =   "Form1.frx":57EFBD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   674
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   311
         Left            =   6960
         Picture         =   "Form1.frx":58198C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   673
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   310
         Left            =   6480
         Picture         =   "Form1.frx":58435B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   672
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   309
         Left            =   6000
         Picture         =   "Form1.frx":586D2A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   671
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   308
         Left            =   5520
         Picture         =   "Form1.frx":5896F9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   670
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   307
         Left            =   5040
         Picture         =   "Form1.frx":58C0C8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   669
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   306
         Left            =   4560
         Picture         =   "Form1.frx":58EA97
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   668
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   305
         Left            =   4080
         Picture         =   "Form1.frx":591466
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   667
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   304
         Left            =   3600
         Picture         =   "Form1.frx":593E35
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   666
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   303
         Left            =   3120
         Picture         =   "Form1.frx":596804
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   665
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   302
         Left            =   2640
         Picture         =   "Form1.frx":5991D3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   664
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   301
         Left            =   2160
         Picture         =   "Form1.frx":59BBA2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   663
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   300
         Left            =   1680
         Picture         =   "Form1.frx":59E571
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   662
         Top             =   6480
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   299
         Left            =   15600
         Picture         =   "Form1.frx":5A0F40
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   661
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   298
         Left            =   15120
         Picture         =   "Form1.frx":5A390F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   660
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   297
         Left            =   14640
         Picture         =   "Form1.frx":5A62DE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   659
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   296
         Left            =   14160
         Picture         =   "Form1.frx":5A8CAD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   658
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   295
         Left            =   13680
         Picture         =   "Form1.frx":5AB67C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   657
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   294
         Left            =   13200
         Picture         =   "Form1.frx":5AE04B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   656
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   293
         Left            =   12720
         Picture         =   "Form1.frx":5B0A1A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   655
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   292
         Left            =   12240
         Picture         =   "Form1.frx":5B33E9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   654
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   291
         Left            =   11760
         Picture         =   "Form1.frx":5B5DB8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   653
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   290
         Left            =   11280
         Picture         =   "Form1.frx":5B8787
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   652
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   289
         Left            =   10800
         Picture         =   "Form1.frx":5BB156
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   651
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   288
         Left            =   10320
         Picture         =   "Form1.frx":5BDB25
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   650
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   287
         Left            =   9840
         Picture         =   "Form1.frx":5C04F4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   649
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   286
         Left            =   9360
         Picture         =   "Form1.frx":5C2EC3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   648
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   285
         Left            =   8880
         Picture         =   "Form1.frx":5C5892
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   647
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   284
         Left            =   8400
         Picture         =   "Form1.frx":5C8261
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   646
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   283
         Left            =   7920
         Picture         =   "Form1.frx":5CAC30
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   645
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   282
         Left            =   7440
         Picture         =   "Form1.frx":5CD5FF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   644
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   281
         Left            =   6960
         Picture         =   "Form1.frx":5CFFCE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   643
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   280
         Left            =   6480
         Picture         =   "Form1.frx":5D299D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   642
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   279
         Left            =   6000
         Picture         =   "Form1.frx":5D536C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   641
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   278
         Left            =   5520
         Picture         =   "Form1.frx":5D7D3B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   640
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   277
         Left            =   5040
         Picture         =   "Form1.frx":5DA70A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   639
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   276
         Left            =   4560
         Picture         =   "Form1.frx":5DD0D9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   638
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   275
         Left            =   4080
         Picture         =   "Form1.frx":5DFAA8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   637
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   274
         Left            =   3600
         Picture         =   "Form1.frx":5E2477
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   636
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   273
         Left            =   3120
         Picture         =   "Form1.frx":5E4E46
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   635
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   272
         Left            =   2640
         Picture         =   "Form1.frx":5E7815
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   634
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   271
         Left            =   2160
         Picture         =   "Form1.frx":5EA1E4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   633
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   270
         Left            =   1680
         Picture         =   "Form1.frx":5ECBB3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   632
         Top             =   6000
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   269
         Left            =   15600
         Picture         =   "Form1.frx":5EF582
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   631
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   268
         Left            =   15120
         Picture         =   "Form1.frx":5F1F51
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   630
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   267
         Left            =   14640
         Picture         =   "Form1.frx":5F4920
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   629
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   266
         Left            =   14160
         Picture         =   "Form1.frx":5F72EF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   628
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   265
         Left            =   13680
         Picture         =   "Form1.frx":5F9CBE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   627
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   264
         Left            =   13200
         Picture         =   "Form1.frx":5FC68D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   626
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   263
         Left            =   12720
         Picture         =   "Form1.frx":5FF05C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   625
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   262
         Left            =   12240
         Picture         =   "Form1.frx":601A2B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   624
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   261
         Left            =   11760
         Picture         =   "Form1.frx":6043FA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   623
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   260
         Left            =   11280
         Picture         =   "Form1.frx":606DC9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   622
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   259
         Left            =   10800
         Picture         =   "Form1.frx":609798
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   621
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   258
         Left            =   10320
         Picture         =   "Form1.frx":60C167
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   620
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   257
         Left            =   9840
         Picture         =   "Form1.frx":60EB36
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   619
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   256
         Left            =   9360
         Picture         =   "Form1.frx":611505
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   618
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   255
         Left            =   8880
         Picture         =   "Form1.frx":613ED4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   617
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   254
         Left            =   8400
         Picture         =   "Form1.frx":6168A3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   616
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   253
         Left            =   7920
         Picture         =   "Form1.frx":619272
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   615
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   252
         Left            =   7440
         Picture         =   "Form1.frx":61BC41
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   614
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   251
         Left            =   6960
         Picture         =   "Form1.frx":61E610
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   613
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   250
         Left            =   6480
         Picture         =   "Form1.frx":620FDF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   612
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   249
         Left            =   6000
         Picture         =   "Form1.frx":6239AE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   611
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   248
         Left            =   5520
         Picture         =   "Form1.frx":62637D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   610
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   247
         Left            =   5040
         Picture         =   "Form1.frx":628D4C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   609
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   246
         Left            =   4560
         Picture         =   "Form1.frx":62B71B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   608
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   245
         Left            =   4080
         Picture         =   "Form1.frx":62E0EA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   607
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   244
         Left            =   3600
         Picture         =   "Form1.frx":630AB9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   606
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   243
         Left            =   3120
         Picture         =   "Form1.frx":633488
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   605
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   242
         Left            =   2640
         Picture         =   "Form1.frx":635E57
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   604
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   241
         Left            =   2160
         Picture         =   "Form1.frx":638826
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   603
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   240
         Left            =   1680
         Picture         =   "Form1.frx":63B1F5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   602
         Top             =   5520
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   239
         Left            =   15600
         Picture         =   "Form1.frx":63DBC4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   601
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   238
         Left            =   15120
         Picture         =   "Form1.frx":640593
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   600
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   237
         Left            =   14640
         Picture         =   "Form1.frx":642F62
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   599
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   236
         Left            =   14160
         Picture         =   "Form1.frx":645931
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   598
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   235
         Left            =   13680
         Picture         =   "Form1.frx":648300
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   597
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   234
         Left            =   13200
         Picture         =   "Form1.frx":64ACCF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   596
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   233
         Left            =   12720
         Picture         =   "Form1.frx":64D69E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   595
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   232
         Left            =   12240
         Picture         =   "Form1.frx":65006D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   594
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   231
         Left            =   11760
         Picture         =   "Form1.frx":652A3C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   593
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   230
         Left            =   11280
         Picture         =   "Form1.frx":65540B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   592
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   229
         Left            =   10800
         Picture         =   "Form1.frx":657DDA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   591
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   228
         Left            =   10320
         Picture         =   "Form1.frx":65A7A9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   590
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   227
         Left            =   9840
         Picture         =   "Form1.frx":65D178
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   589
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   226
         Left            =   9360
         Picture         =   "Form1.frx":65FB47
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   588
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   225
         Left            =   8880
         Picture         =   "Form1.frx":662516
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   587
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   224
         Left            =   8400
         Picture         =   "Form1.frx":664EE5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   586
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   223
         Left            =   7920
         Picture         =   "Form1.frx":6678B4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   585
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   222
         Left            =   7440
         Picture         =   "Form1.frx":66A283
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   584
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   221
         Left            =   6960
         Picture         =   "Form1.frx":66CC52
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   583
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   220
         Left            =   6480
         Picture         =   "Form1.frx":66F621
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   582
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   219
         Left            =   6000
         Picture         =   "Form1.frx":671FF0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   581
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   218
         Left            =   5520
         Picture         =   "Form1.frx":6749BF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   580
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   217
         Left            =   5040
         Picture         =   "Form1.frx":67738E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   579
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   216
         Left            =   4560
         Picture         =   "Form1.frx":679D5D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   578
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   215
         Left            =   4080
         Picture         =   "Form1.frx":67C72C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   577
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   214
         Left            =   3600
         Picture         =   "Form1.frx":67F0FB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   576
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   213
         Left            =   3120
         Picture         =   "Form1.frx":681ACA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   575
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   212
         Left            =   2640
         Picture         =   "Form1.frx":684499
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   574
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   211
         Left            =   2160
         Picture         =   "Form1.frx":686E68
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   573
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   210
         Left            =   1680
         Picture         =   "Form1.frx":689837
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   572
         Top             =   5040
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   209
         Left            =   15600
         Picture         =   "Form1.frx":68C206
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   571
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   208
         Left            =   15120
         Picture         =   "Form1.frx":68EBD5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   570
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   207
         Left            =   14640
         Picture         =   "Form1.frx":6915A4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   569
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   206
         Left            =   14160
         Picture         =   "Form1.frx":693F73
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   568
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   205
         Left            =   13680
         Picture         =   "Form1.frx":696942
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   567
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   204
         Left            =   13200
         Picture         =   "Form1.frx":699311
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   566
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   203
         Left            =   12720
         Picture         =   "Form1.frx":69BCE0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   565
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   202
         Left            =   12240
         Picture         =   "Form1.frx":69E6AF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   564
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   201
         Left            =   11760
         Picture         =   "Form1.frx":6A107E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   563
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   200
         Left            =   11280
         Picture         =   "Form1.frx":6A3A4D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   562
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   199
         Left            =   10800
         Picture         =   "Form1.frx":6A641C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   561
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   198
         Left            =   10320
         Picture         =   "Form1.frx":6A8DEB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   560
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   197
         Left            =   9840
         Picture         =   "Form1.frx":6AB7BA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   559
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   196
         Left            =   9360
         Picture         =   "Form1.frx":6AE189
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   558
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   195
         Left            =   8880
         Picture         =   "Form1.frx":6B0B58
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   557
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   194
         Left            =   8400
         Picture         =   "Form1.frx":6B3527
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   556
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   193
         Left            =   7920
         Picture         =   "Form1.frx":6B5EF6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   555
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   192
         Left            =   7440
         Picture         =   "Form1.frx":6B88C5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   554
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   191
         Left            =   6960
         Picture         =   "Form1.frx":6BB294
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   553
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   190
         Left            =   6480
         Picture         =   "Form1.frx":6BDC63
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   552
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   189
         Left            =   6000
         Picture         =   "Form1.frx":6C0632
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   551
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   188
         Left            =   5520
         Picture         =   "Form1.frx":6C3001
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   550
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   187
         Left            =   5040
         Picture         =   "Form1.frx":6C59D0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   549
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   186
         Left            =   4560
         Picture         =   "Form1.frx":6C839F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   548
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   185
         Left            =   4080
         Picture         =   "Form1.frx":6CAD6E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   547
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   184
         Left            =   3600
         Picture         =   "Form1.frx":6CD73D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   546
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   183
         Left            =   3120
         Picture         =   "Form1.frx":6D010C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   545
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   182
         Left            =   2640
         Picture         =   "Form1.frx":6D2ADB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   544
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   181
         Left            =   2160
         Picture         =   "Form1.frx":6D54AA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   543
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   180
         Left            =   1680
         Picture         =   "Form1.frx":6D7E79
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   542
         Top             =   4560
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   179
         Left            =   15600
         Picture         =   "Form1.frx":6DA848
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   541
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   178
         Left            =   15120
         Picture         =   "Form1.frx":6DD217
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   540
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   177
         Left            =   14640
         Picture         =   "Form1.frx":6DFBE6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   539
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   176
         Left            =   14160
         Picture         =   "Form1.frx":6E25B5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   538
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   175
         Left            =   13680
         Picture         =   "Form1.frx":6E4F84
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   537
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   174
         Left            =   13200
         Picture         =   "Form1.frx":6E7953
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   536
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   173
         Left            =   12720
         Picture         =   "Form1.frx":6EA322
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   535
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   172
         Left            =   12240
         Picture         =   "Form1.frx":6ECCF1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   534
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   171
         Left            =   11760
         Picture         =   "Form1.frx":6EF6C0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   533
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   170
         Left            =   11280
         Picture         =   "Form1.frx":6F208F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   532
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   169
         Left            =   10800
         Picture         =   "Form1.frx":6F4A5E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   531
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   168
         Left            =   10320
         Picture         =   "Form1.frx":6F742D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   530
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   167
         Left            =   9840
         Picture         =   "Form1.frx":6F9DFC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   529
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   166
         Left            =   9360
         Picture         =   "Form1.frx":6FC7CB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   528
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   165
         Left            =   8880
         Picture         =   "Form1.frx":6FF19A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   527
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   164
         Left            =   8400
         Picture         =   "Form1.frx":701B69
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   526
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   163
         Left            =   7920
         Picture         =   "Form1.frx":704538
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   525
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   162
         Left            =   7440
         Picture         =   "Form1.frx":706F07
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   524
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   161
         Left            =   6960
         Picture         =   "Form1.frx":7098D6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   523
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   160
         Left            =   6480
         Picture         =   "Form1.frx":70C2A5
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   522
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   159
         Left            =   6000
         Picture         =   "Form1.frx":70EC74
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   521
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   158
         Left            =   5520
         Picture         =   "Form1.frx":711643
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   520
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   157
         Left            =   5040
         Picture         =   "Form1.frx":714012
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   519
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   156
         Left            =   4560
         Picture         =   "Form1.frx":7169E1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   518
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   155
         Left            =   4080
         Picture         =   "Form1.frx":7193B0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   517
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   154
         Left            =   3600
         Picture         =   "Form1.frx":71BD7F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   516
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   153
         Left            =   3120
         Picture         =   "Form1.frx":71E74E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   515
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   152
         Left            =   2640
         Picture         =   "Form1.frx":72111D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   514
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   151
         Left            =   2160
         Picture         =   "Form1.frx":723AEC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   513
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   150
         Left            =   1680
         Picture         =   "Form1.frx":7264BB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   512
         Top             =   4080
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   149
         Left            =   15600
         Picture         =   "Form1.frx":728E8A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   511
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   148
         Left            =   15120
         Picture         =   "Form1.frx":72B859
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   510
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   147
         Left            =   14640
         Picture         =   "Form1.frx":72E228
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   509
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   146
         Left            =   14160
         Picture         =   "Form1.frx":730BF7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   508
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   145
         Left            =   13680
         Picture         =   "Form1.frx":7335C6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   507
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   144
         Left            =   13200
         Picture         =   "Form1.frx":735F95
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   506
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   143
         Left            =   12720
         Picture         =   "Form1.frx":738964
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   505
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   142
         Left            =   12240
         Picture         =   "Form1.frx":73B333
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   504
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   141
         Left            =   11760
         Picture         =   "Form1.frx":73DD02
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   503
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   140
         Left            =   11280
         Picture         =   "Form1.frx":7406D1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   502
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   139
         Left            =   10800
         Picture         =   "Form1.frx":7430A0
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   501
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   138
         Left            =   10320
         Picture         =   "Form1.frx":745A6F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   500
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   137
         Left            =   9840
         Picture         =   "Form1.frx":74843E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   499
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   136
         Left            =   9360
         Picture         =   "Form1.frx":74AE0D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   498
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   135
         Left            =   8880
         Picture         =   "Form1.frx":74D7DC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   497
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   134
         Left            =   8400
         Picture         =   "Form1.frx":7501AB
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   496
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   133
         Left            =   7920
         Picture         =   "Form1.frx":752B7A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   495
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   132
         Left            =   7440
         Picture         =   "Form1.frx":755549
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   494
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   131
         Left            =   6960
         Picture         =   "Form1.frx":757F18
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   493
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   130
         Left            =   6480
         Picture         =   "Form1.frx":75A8E7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   492
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   129
         Left            =   6000
         Picture         =   "Form1.frx":75D2B6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   491
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   128
         Left            =   5520
         Picture         =   "Form1.frx":75FC85
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   490
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   127
         Left            =   5040
         Picture         =   "Form1.frx":762654
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   489
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   126
         Left            =   4560
         Picture         =   "Form1.frx":765023
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   488
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   125
         Left            =   4080
         Picture         =   "Form1.frx":7679F2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   487
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   124
         Left            =   3600
         Picture         =   "Form1.frx":76A3C1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   486
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   123
         Left            =   3120
         Picture         =   "Form1.frx":76CD90
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   485
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   122
         Left            =   2640
         Picture         =   "Form1.frx":76F75F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   484
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   121
         Left            =   2160
         Picture         =   "Form1.frx":77212E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   483
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   120
         Left            =   1680
         Picture         =   "Form1.frx":774AFD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   482
         Top             =   3600
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   119
         Left            =   15600
         Picture         =   "Form1.frx":7774CC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   481
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   118
         Left            =   15120
         Picture         =   "Form1.frx":779E9B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   480
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   117
         Left            =   14640
         Picture         =   "Form1.frx":77C86A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   479
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   116
         Left            =   14160
         Picture         =   "Form1.frx":77F239
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   478
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   115
         Left            =   13680
         Picture         =   "Form1.frx":781C08
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   477
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   114
         Left            =   13200
         Picture         =   "Form1.frx":7845D7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   476
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   113
         Left            =   12720
         Picture         =   "Form1.frx":786FA6
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   475
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   112
         Left            =   12240
         Picture         =   "Form1.frx":789975
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   474
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   111
         Left            =   11760
         Picture         =   "Form1.frx":78C344
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   473
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   110
         Left            =   11280
         Picture         =   "Form1.frx":78ED13
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   472
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   109
         Left            =   10800
         Picture         =   "Form1.frx":7916E2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   471
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   108
         Left            =   10320
         Picture         =   "Form1.frx":7940B1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   470
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   107
         Left            =   9840
         Picture         =   "Form1.frx":796A80
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   469
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   106
         Left            =   9360
         Picture         =   "Form1.frx":79944F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   468
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   105
         Left            =   8880
         Picture         =   "Form1.frx":79BE1E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   467
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   104
         Left            =   8400
         Picture         =   "Form1.frx":79E7ED
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   466
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   103
         Left            =   7920
         Picture         =   "Form1.frx":7A11BC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   465
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   102
         Left            =   7440
         Picture         =   "Form1.frx":7A3B8B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   464
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   101
         Left            =   6960
         Picture         =   "Form1.frx":7A655A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   463
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   100
         Left            =   6480
         Picture         =   "Form1.frx":7A8F29
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   462
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   99
         Left            =   6000
         Picture         =   "Form1.frx":7AB8F8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   461
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   98
         Left            =   5520
         Picture         =   "Form1.frx":7AE2C7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   460
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   97
         Left            =   5040
         Picture         =   "Form1.frx":7B0C96
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   459
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   96
         Left            =   4560
         Picture         =   "Form1.frx":7B3665
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   458
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   95
         Left            =   4080
         Picture         =   "Form1.frx":7B6034
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   457
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   94
         Left            =   3600
         Picture         =   "Form1.frx":7B8A03
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   456
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   93
         Left            =   3120
         Picture         =   "Form1.frx":7BB3D2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   455
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   92
         Left            =   2640
         Picture         =   "Form1.frx":7BDDA1
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   454
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   91
         Left            =   2160
         Picture         =   "Form1.frx":7C0770
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   453
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   90
         Left            =   1680
         Picture         =   "Form1.frx":7C313F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   452
         Top             =   3120
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   89
         Left            =   15600
         Picture         =   "Form1.frx":7C5B0E
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   451
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   88
         Left            =   15120
         Picture         =   "Form1.frx":7C84DD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   450
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   87
         Left            =   14640
         Picture         =   "Form1.frx":7CAEAC
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   449
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   86
         Left            =   14160
         Picture         =   "Form1.frx":7CD87B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   448
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   85
         Left            =   13680
         Picture         =   "Form1.frx":7D024A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   447
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   84
         Left            =   13200
         Picture         =   "Form1.frx":7D2C19
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   446
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   83
         Left            =   12720
         Picture         =   "Form1.frx":7D55E8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   445
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   82
         Left            =   12240
         Picture         =   "Form1.frx":7D7FB7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   444
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   81
         Left            =   11760
         Picture         =   "Form1.frx":7DA986
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   443
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   80
         Left            =   11280
         Picture         =   "Form1.frx":7DD355
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   442
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   79
         Left            =   10800
         Picture         =   "Form1.frx":7DFD24
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   441
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   78
         Left            =   10320
         Picture         =   "Form1.frx":7E26F3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   440
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   77
         Left            =   9840
         Picture         =   "Form1.frx":7E50C2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   439
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   76
         Left            =   9360
         Picture         =   "Form1.frx":7E7A91
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   438
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   75
         Left            =   8880
         Picture         =   "Form1.frx":7EA460
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   437
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   74
         Left            =   8400
         Picture         =   "Form1.frx":7ECE2F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   436
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   73
         Left            =   7920
         Picture         =   "Form1.frx":7EF7FE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   435
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   72
         Left            =   7440
         Picture         =   "Form1.frx":7F21CD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   434
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   71
         Left            =   6960
         Picture         =   "Form1.frx":7F4B9C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   433
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   70
         Left            =   6480
         Picture         =   "Form1.frx":7F756B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   432
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   69
         Left            =   6000
         Picture         =   "Form1.frx":7F9F3A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   431
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   68
         Left            =   5520
         Picture         =   "Form1.frx":7FC909
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   430
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   67
         Left            =   5040
         Picture         =   "Form1.frx":7FF2D8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   429
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   66
         Left            =   4560
         Picture         =   "Form1.frx":801CA7
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   428
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   65
         Left            =   4080
         Picture         =   "Form1.frx":804676
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   427
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   64
         Left            =   3600
         Picture         =   "Form1.frx":807045
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   426
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   63
         Left            =   3120
         Picture         =   "Form1.frx":809A14
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   425
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   62
         Left            =   2640
         Picture         =   "Form1.frx":80C3E3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   424
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   61
         Left            =   2160
         Picture         =   "Form1.frx":80EDB2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   423
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   60
         Left            =   1680
         Picture         =   "Form1.frx":811781
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   422
         Top             =   2640
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   59
         Left            =   15600
         Picture         =   "Form1.frx":814150
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   421
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   58
         Left            =   15120
         Picture         =   "Form1.frx":816B1F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   420
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   57
         Left            =   14640
         Picture         =   "Form1.frx":8194EE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   419
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   56
         Left            =   14160
         Picture         =   "Form1.frx":81BEBD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   418
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   55
         Left            =   13680
         Picture         =   "Form1.frx":81E88C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   417
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   54
         Left            =   13200
         Picture         =   "Form1.frx":82125B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   416
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   53
         Left            =   12720
         Picture         =   "Form1.frx":823C2A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   415
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   52
         Left            =   12240
         Picture         =   "Form1.frx":8265F9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   414
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   51
         Left            =   11760
         Picture         =   "Form1.frx":828FC8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   413
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   50
         Left            =   11280
         Picture         =   "Form1.frx":82B997
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   412
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   49
         Left            =   10800
         Picture         =   "Form1.frx":82E366
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   411
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   48
         Left            =   10320
         Picture         =   "Form1.frx":830D35
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   410
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   47
         Left            =   9840
         Picture         =   "Form1.frx":833704
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   409
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   46
         Left            =   9360
         Picture         =   "Form1.frx":8360D3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   408
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   45
         Left            =   8880
         Picture         =   "Form1.frx":838AA2
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   407
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   44
         Left            =   8400
         Picture         =   "Form1.frx":83B471
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   406
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   43
         Left            =   7920
         Picture         =   "Form1.frx":83DE40
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   405
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   42
         Left            =   7440
         Picture         =   "Form1.frx":84080F
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   404
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   41
         Left            =   6960
         Picture         =   "Form1.frx":8431DE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   403
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   40
         Left            =   6480
         Picture         =   "Form1.frx":845BAD
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   402
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   39
         Left            =   6000
         Picture         =   "Form1.frx":84857C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   401
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   38
         Left            =   5520
         Picture         =   "Form1.frx":84AF4B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   400
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   37
         Left            =   5040
         Picture         =   "Form1.frx":84D91A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   399
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   36
         Left            =   4560
         Picture         =   "Form1.frx":8502E9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   398
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   35
         Left            =   4080
         Picture         =   "Form1.frx":852CB8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   397
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   34
         Left            =   3600
         Picture         =   "Form1.frx":855687
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   396
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   33
         Left            =   3120
         Picture         =   "Form1.frx":858056
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   395
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   32
         Left            =   2640
         Picture         =   "Form1.frx":85AA25
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   394
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   31
         Left            =   2160
         Picture         =   "Form1.frx":85D3F4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   392
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   30
         Left            =   1680
         Picture         =   "Form1.frx":85FDC3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   391
         Top             =   2160
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   29
         Left            =   15600
         Picture         =   "Form1.frx":862792
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   390
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   28
         Left            =   15120
         Picture         =   "Form1.frx":865161
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   389
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   27
         Left            =   14640
         Picture         =   "Form1.frx":867B30
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   388
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   26
         Left            =   14160
         Picture         =   "Form1.frx":86A4FF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   387
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   25
         Left            =   13680
         Picture         =   "Form1.frx":86CECE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   386
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   24
         Left            =   13200
         Picture         =   "Form1.frx":86F89D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   385
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   23
         Left            =   12720
         Picture         =   "Form1.frx":87226C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   384
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   22
         Left            =   12240
         Picture         =   "Form1.frx":874C3B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   383
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   21
         Left            =   11760
         Picture         =   "Form1.frx":87760A
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   382
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   20
         Left            =   11280
         Picture         =   "Form1.frx":879FD9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   381
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   19
         Left            =   10800
         Picture         =   "Form1.frx":87C9A8
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   380
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   18
         Left            =   10320
         Picture         =   "Form1.frx":87F377
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   379
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   17
         Left            =   9840
         Picture         =   "Form1.frx":881D46
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   378
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   16
         Left            =   9360
         Picture         =   "Form1.frx":884715
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   377
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   15
         Left            =   8880
         Picture         =   "Form1.frx":8870E4
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   376
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   14
         Left            =   8400
         Picture         =   "Form1.frx":889AB3
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   375
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   13
         Left            =   7920
         Picture         =   "Form1.frx":88C482
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   374
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   12
         Left            =   7440
         Picture         =   "Form1.frx":88EE51
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   373
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   11
         Left            =   6960
         Picture         =   "Form1.frx":891820
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   372
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   10
         Left            =   6480
         Picture         =   "Form1.frx":8941EF
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   371
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   9
         Left            =   6000
         Picture         =   "Form1.frx":896BBE
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   370
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   8
         Left            =   5520
         Picture         =   "Form1.frx":89958D
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   369
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   7
         Left            =   5040
         Picture         =   "Form1.frx":89BF5C
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   368
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   6
         Left            =   4560
         Picture         =   "Form1.frx":89E92B
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   367
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   5
         Left            =   4080
         Picture         =   "Form1.frx":8A12FA
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   366
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   4
         Left            =   3600
         Picture         =   "Form1.frx":8A3CC9
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   365
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   3
         Left            =   3120
         Picture         =   "Form1.frx":8A6698
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   364
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   2
         Left            =   2640
         Picture         =   "Form1.frx":8A9067
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   363
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   1
         Left            =   2160
         Picture         =   "Form1.frx":8ABA36
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   362
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox picex 
         BackColor       =   &H80000008&
         BorderStyle     =   0  'None
         Height          =   545
         Index           =   0
         Left            =   1680
         Picture         =   "Form1.frx":8AE405
         ScaleHeight     =   540
         ScaleWidth      =   540
         TabIndex        =   361
         Top             =   1680
         Width           =   540
      End
      Begin VB.PictureBox startex 
         AutoSize        =   -1  'True
         Height          =   750
         Left            =   8520
         Picture         =   "Form1.frx":8B0DD4
         ScaleHeight     =   690
         ScaleWidth      =   690
         TabIndex        =   360
         Top             =   360
         Width           =   750
      End
      Begin VB.Label Labeltex 
         Alignment       =   2  'Center
         BackStyle       =   0  'Transparent
         Caption         =   "0"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   18
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000E&
         Height          =   375
         Left            =   14520
         TabIndex        =   843
         Top             =   600
         Width           =   1095
      End
      Begin VB.Label Labelmex 
         Alignment       =   2  'Center
         BackStyle       =   0  'Transparent
         Caption         =   "99"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   18
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000E&
         Height          =   375
         Left            =   2280
         TabIndex        =   842
         Top             =   600
         Width           =   1095
      End
   End
   Begin VB.Menu game 
      Caption         =   "Game"
      Begin VB.Menu newgame 
         Caption         =   "New"
         Shortcut        =   {F2}
      End
      Begin VB.Menu dash1 
         Caption         =   "-"
      End
      Begin VB.Menu beginner 
         Caption         =   "Beginner"
      End
      Begin VB.Menu intermediate 
         Caption         =   "Intermediate"
      End
      Begin VB.Menu expert 
         Caption         =   "Expert"
      End
      Begin VB.Menu dash2 
         Caption         =   "-"
      End
      Begin VB.Menu besttimes 
         Caption         =   "Best Times..."
      End
      Begin VB.Menu dash3 
         Caption         =   "-"
      End
      Begin VB.Menu exit 
         Caption         =   "Exit"
      End
   End
   Begin VB.Menu help 
      Caption         =   "Help"
      Begin VB.Menu contents 
         Caption         =   "Contents..."
      End
      Begin VB.Menu about 
         Caption         =   "About..."
      End
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim l, r, iffi, allow, panduanlr As Boolean ' l for left button pressed, and r for right
'allow for whether player can play game at this time,panduanlr for judeg whether the two button both be pressed also see cishulr
'iffi for whether this is the first time that the player play the game, and it can reset mine if player click min at the first time
Private Type besttime
    t As Integer
    n As String * 25
End Type
Dim fastesttime(2) As besttime
Private Type modeini
    a As Integer
End Type
Dim degreemode As modeini
Dim b1(8, 8), b2(8, 8), i1(15, 15), i2(15, 15), e1(15, 29), e2(15, 29), de, mine, curpoint, cishulr, stime As Integer 'curpoint is used to clearing flash

Private Sub about_Click()
    Load Form5
    Form5.Show
End Sub

Private Sub beginner_Click()
    beginner.Checked = True
    expert.Checked = Falset
    intermediate.Checked = False
    degreemode.a = 1
    Open App.Path & "\llini.lldat" For Random As #1
    Put #1, 1, degreemode
    Close #1
    Call beg
End Sub

Public Sub besttimes_Click()
   Call Form3.fa(fastesttime(0).t, fastesttime(0).n, fastesttime(1).t, fastesttime(1).n, fastesttime(2).t, fastesttime(2).n)
    Form3.Show
End Sub

Private Sub betextname_KeyDown(KeyCode As Integer, Shift As Integer)
    If KeyCode = 13 Then Call Command1_Click
End Sub

Private Sub Command1_Click()
    Call berecord(stime, betextname.Text)
    befastest.Visible = False
End Sub

Private Sub Command2_Click()
 Call inrecord(stime, intextname.Text)
    infastest.Visible = False
End Sub

Private Sub Command3_Click()
    Call exrecord(stime, extextname.Text)
    exfastest.Visible = False
End Sub

Private Sub contents_Click()
    Load Form4
    Form4.Show
End Sub

Private Sub exit_Click()
    Unload Me
End Sub

Private Sub expert_Click()
     beginner.Checked = False
    expert.Checked = True
    intermediate.Checked = False
    degreemode.a = 3
    Open App.Path & "\llini.lldat" For Random As #1
    Put #1, 1, degreemode
    Close #1
    Call exper
End Sub

Private Sub extextname_KeyDown(KeyCode As Integer, Shift As Integer)
    If KeyCode = 13 Then Call Command3_Click
End Sub



Private Sub Form_Load()
    Unload Form4
    Unload Form5
    Unload Form6
    Call formini
End Sub
Public Sub ini()     'initialize
    Select Case de
        Case 1
            For i = 0 To 8
                For j = 0 To 8
                    b1(i, j) = 0
                    b2(i, j) = 0
                Next j
            Next i
        Case 2
            For i = 0 To 15
                For j = 0 To 15
                    i1(i, j) = 0
                    i2(i, j) = 0
                Next j
            Next i
        Case 3
             For i = 0 To 15
                For j = 0 To 29
                    e1(i, j) = 0
                    e2(i, j) = 0
                Next j
            Next i
        End Select
    iffi = True
    panduanlr = False
    cishulr = 0
    stime = 0
    l = False
    r = False
    startbe.Picture = Picture1(15).Picture
    startin.Picture = Picture1(15).Picture
    startex.Picture = Picture1(15).Picture
    Timer1.Enabled = False
End Sub

Public Sub dis()
    Select Case de
        Case 1
            p = 9
            For i = 0 To 8
                For j = 0 To 8
                    If b2(i, j) = 0 Then
                        picbe(i * p + j).Picture = Picture1(0).Picture
                    'ElseIf b2(i, j) = -1 Then
                        'picbe(i * p + j).Picture = Picture1(12).Picture
                    ElseIf b2(i, j) = 2 Then
                        picbe(i * p + j).Picture = Picture1(10).Picture
                    'ElseIf b2(i, j) = -2 Then
                        'picbe(i * p + j).Picture = Picture1(11).Picture
                    ElseIf b2(i, j) = 1 Then
                        Select Case b1(i, j)
                            Case 0
                                 picbe(i * p + j).Picture = Picture1(9).Picture
                            Case 1
                                 picbe(i * p + j).Picture = Picture1(1).Picture
                            Case 2
                                 picbe(i * p + j).Picture = Picture1(2).Picture
                            Case 3
                                 picbe(i * p + j).Picture = Picture1(3).Picture
                            Case 4
                                 picbe(i * p + j).Picture = Picture1(4).Picture
                            Case 5
                                 picbe(i * p + j).Picture = Picture1(5).Picture
                            Case 6
                                 picbe(i * p + j).Picture = Picture1(6).Picture
                            Case 7
                                 picbe(i * p + j).Picture = Picture1(7).Picture
                            Case 8
                                 picbe(i * p + j).Picture = Picture1(8).Picture
                           Case -1
                                picbe(i * p + j).Picture = Picture1(13).Picture
                        End Select
                    End If
                Next j
            Next i
        Case 2
            p = 16
            For i = 0 To 15
                For j = 0 To 15
                    If i2(i, j) = 0 Then
                        picin(i * p + j).Picture = Picture1(0).Picture
                    'ElseIf i2(i, j) = -1 Then
                        'picin(i * p + j).Picture = Picture1(12).Picture
                    ElseIf i2(i, j) = 2 Then
                        picin(i * p + j).Picture = Picture1(10).Picture
                    'ElseIf i2(i, j) = -2 Then
                        'picin(i * p + j).Picture = Picture1(11).Picture
                    ElseIf i2(i, j) = 1 Then
                        Select Case i1(i, j)
                            Case 0
                                 picin(i * p + j).Picture = Picture1(9).Picture
                            Case 1
                                 picin(i * p + j).Picture = Picture1(1).Picture
                            Case 2
                                 picin(i * p + j).Picture = Picture1(2).Picture
                            Case 3
                                 picin(i * p + j).Picture = Picture1(3).Picture
                            Case 4
                                 picin(i * p + j).Picture = Picture1(4).Picture
                            Case 5
                                 picin(i * p + j).Picture = Picture1(5).Picture
                            Case 6
                                 picin(i * p + j).Picture = Picture1(6).Picture
                            Case 7
                                 picin(i * p + j).Picture = Picture1(7).Picture
                            Case 8
                                 picin(i * p + j).Picture = Picture1(8).Picture
                             'Case -1
                                'picin(i * p + j).Picture = Picture1(13).Picture
                        End Select
                    End If
                Next j
            Next i
        Case 3
            p = 30
            For i = 0 To 15
                For j = 0 To 29
                    If e2(i, j) = 0 Then
                        picex(i * p + j).Picture = Picture1(0).Picture
                    'ElseIf e2(i, j) = -1 Then
                        'picex(i * p + j).Picture = Picture1(12).Picture
                    ElseIf e2(i, j) = 2 Then
                        picex(i * p + j).Picture = Picture1(10).Picture
                    'ElseIf e2(i, j) = -2 Then
                        'picex(i * p + j).Picture = Picture1(11).Picture
                    ElseIf e2(i, j) = 1 Then
                        Select Case e1(i, j)
                            Case 0
                                 picex(i * p + j).Picture = Picture1(9).Picture
                            Case 1
                                 picex(i * p + j).Picture = Picture1(1).Picture
                            Case 2
                                 picex(i * p + j).Picture = Picture1(2).Picture
                            Case 3
                                 picex(i * p + j).Picture = Picture1(3).Picture
                            Case 4
                                 picex(i * p + j).Picture = Picture1(4).Picture
                            Case 5
                                 picex(i * p + j).Picture = Picture1(5).Picture
                            Case 6
                                 picex(i * p + j).Picture = Picture1(6).Picture
                            Case 7
                                 picex(i * p + j).Picture = Picture1(7).Picture
                            Case 8
                                 picex(i * p + j).Picture = Picture1(8).Picture
                            ' Case -1
                                'picex(i * p + j).Picture = Picture1(13).Picture
                        End Select
                    End If
                Next j
            Next i
    End Select
End Sub

Public Sub generatemine()
    Dim r, c As Integer
    Select Case de
        Case 1
            For i = 1 To 10
                Randomize
                Do
                    r = Int(Rnd() * 9)
                    c = Int(Rnd() * 9)
                    If b1(r, c) = 0 Then
                        b1(r, c) = -1
                        Exit Do
                    End If
                Loop
            Next
        Case 2
            For i = 1 To 40
                Randomize
                Do
                    r = Int(Rnd() * 16)
                    c = Int(Rnd() * 16)
                    If i1(r, c) = 0 Then
                        i1(r, c) = -1
                        Exit Do
                    End If
                Loop
            Next
        Case 3
            For i = 1 To 99
                Randomize
                Do
                    r = Int(Rnd() * 16)
                    c = Int(Rnd() * 30)
                    If e1(r, c) = 0 Then
                        e1(r, c) = -1
                        Exit Do
                    End If
                Loop
            Next
    End Select
End Sub

Public Sub start()
    Call generatemine
    Dim mm As Integer
    mm = 0
    Select Case de
        Case 1
            For i = 0 To 8
                For j = 0 To 8
                    If b1(i, j) = 0 Then
                        mm = 0
                        If i - 1 >= 0 And j - 1 >= 0 Then mm = mm + IIf(b1(i - 1, j - 1) = -1, 1, 0)
                        If i - 1 >= 0 Then mm = mm + IIf(b1(i - 1, j) = -1, 1, 0)
                        If i - 1 >= 0 And j + 1 <= 8 Then mm = mm + IIf(b1(i - 1, j + 1) = -1, 1, 0)
                        If j - 1 >= 0 Then mm = mm + IIf(b1(i, j - 1) = -1, 1, 0)
                        If j + 1 <= 8 Then mm = mm + IIf(b1(i, j + 1) = -1, 1, 0)
                        If i + 1 <= 8 And j - 1 >= 0 Then mm = mm + IIf(b1(i + 1, j - 1) = -1, 1, 0)
                        If i + 1 <= 8 Then mm = mm + IIf(b1(i + 1, j) = -1, 1, 0)
                        If i + 1 <= 8 And j + 1 <= 8 Then mm = mm + IIf(b1(i + 1, j + 1) = -1, 1, 0)
                        b1(i, j) = mm
                    End If
                Next j
            Next i
        Case 2
            For i = 0 To 15
                For j = 0 To 15
                    If i1(i, j) = 0 Then
                        mm = 0
                        If i - 1 >= 0 And j - 1 >= 0 Then mm = mm + IIf(i1(i - 1, j - 1) = -1, 1, 0)
                        If i - 1 >= 0 Then mm = mm + IIf(i1(i - 1, j) = -1, 1, 0)
                        If i - 1 >= 0 And j + 1 <= 15 Then mm = mm + IIf(i1(i - 1, j + 1) = -1, 1, 0)
                        If j - 1 >= 0 Then mm = mm + IIf(i1(i, j - 1) = -1, 1, 0)
                        If j + 1 <= 15 Then mm = mm + IIf(i1(i, j + 1) = -1, 1, 0)
                        If i + 1 <= 15 And j - 1 >= 0 Then mm = mm + IIf(i1(i + 1, j - 1) = -1, 1, 0)
                        If i + 1 <= 15 Then mm = mm + IIf(i1(i + 1, j) = -1, 1, 0)
                        If i + 1 <= 15 And j + 1 <= 15 Then mm = mm + IIf(i1(i + 1, j + 1) = -1, 1, 0)
                        i1(i, j) = mm
                    End If
                Next j
            Next i
        Case 3
            For i = 0 To 15
                For j = 0 To 29
                    If e1(i, j) = 0 Then
                        mm = 0
                        If i - 1 >= 0 And j - 1 >= 0 Then mm = mm + IIf(e1(i - 1, j - 1) = -1, 1, 0)
                        If i - 1 >= 0 Then mm = mm + IIf(e1(i - 1, j) = -1, 1, 0)
                        If i - 1 >= 0 And j + 1 <= 29 Then mm = mm + IIf(e1(i - 1, j + 1) = -1, 1, 0)
                        If j - 1 >= 0 Then mm = mm + IIf(e1(i, j - 1) = -1, 1, 0)
                        If j + 1 <= 29 Then mm = mm + IIf(e1(i, j + 1) = -1, 1, 0)
                        If i + 1 <= 15 And j - 1 >= 0 Then mm = mm + IIf(e1(i + 1, j - 1) = -1, 1, 0)
                        If i + 1 <= 15 Then mm = mm + IIf(e1(i + 1, j) = -1, 1, 0)
                        If i + 1 <= 15 And j + 1 <= 29 Then mm = mm + IIf(e1(i + 1, j + 1) = -1, 1, 0)
                        e1(i, j) = mm
                    End If
                Next j
            Next i
    End Select
End Sub

Public Sub lose(a As Integer)
    allow = False
    Timer1.Enabled = False
    Dim rr, c, cc As Integer
    Select Case de
        Case 1
            cc = 9
        Case 2
            cc = 16
        Case 3
            cc = 30
    End Select
    rr = a \ cc
    c = a Mod cc
    Select Case de
        Case 1
            startbe.Picture = Picture1(17).Picture
            b1(rr, c) = -2
            picbe(a).Picture = Picture1(14).Picture
            For i = 0 To 8
                For j = 0 To 8
                    If b1(i, j) = -1 And b2(i, j) = 0 Then
                        picbe(i * cc + j).Picture = Picture1(13).Picture
                    ElseIf b1(i, j) <> -1 And b2(i, j) = 2 Then
                        picbe(i * cc + j).Picture = Picture1(19).Picture
                    End If
                Next j
            Next i
        Case 2
            startin.Picture = Picture1(17).Picture
            i1(rr, c) = -2
            picin(a).Picture = Picture1(14).Picture
            For i = 0 To 15
                For j = 0 To 15
                    If i1(i, j) = -1 And i2(i, j) = 0 Then
                        picin(i * cc + j).Picture = Picture1(13).Picture
                    ElseIf i1(i, j) <> -1 And i2(i, j) = 2 Then
                        picin(i * cc + j).Picture = Picture1(19).Picture
                    End If
                Next j
            Next i
        Case 3
            startex.Picture = Picture1(17).Picture
            e1(rr, c) = -2
            picex(a).Picture = Picture1(14).Picture
            For i = 0 To 15
                For j = 0 To 29
                    If e1(i, j) = -1 And e2(i, j) = 0 Then
                        picex(i * cc + j).Picture = Picture1(13).Picture
                    ElseIf e1(i, j) <> -1 And e2(i, j) = 2 Then
                        picex(i * cc + j).Picture = Picture1(19).Picture
                    End If
                Next j
            Next i
    End Select
End Sub

Public Sub beg()
    Pictureb.Visible = True
    Picturei.Visible = False
    Picturee.Visible = False
    Width = Pictureb.ScaleWidth
    Height = Pictureb.ScaleHeight
    Pictureb.Left = 0
    Pictureb.Top = 0
    de = 1
    Call ini
    Call start
    Call dis
    allow = True
    curpoint = -1
    labeltbe.Caption = Str(stime)
    labelmbe.Caption = Str(10)
    Form2.Cls
    For i = 0 To 8
        For j = 0 To 8
            Form2.Print Format(Str(b1(i, j)), "@@") & ",";
        Next
        Form2.Print
    Next
End Sub

Public Sub inter()
    Pictureb.Visible = False
    Picturei.Visible = True
    Picturee.Visible = False
    Width = Picturei.ScaleWidth
    Height = Picturei.ScaleHeight
    Picturei.Left = 0
    Picturei.Top = 0
    de = 2
    Call ini
    Call start
    Call dis
    allow = True
    curpoint = -1
    Labelmin.Caption = Str(40)
    Labeltin.Caption = Str(stime)
    Form2.Cls
    For i = 0 To 15
        For j = 0 To 15
            Form2.Print Format(Str(i1(i, j)), "@@") & ",";
        Next
        Form2.Print
    Next
End Sub

Public Sub exper()
    Pictureb.Visible = False
    Picturei.Visible = False
    Picturee.Visible = True
    Width = Picturee.ScaleWidth
    Height = Picturee.ScaleHeight
    Picturee.Left = 0
    Picturee.Top = 0
    de = 3
    Call ini
    Call start
    Call dis
    allow = True
    curpoint = -1
    Labelmex.Caption = Str(99)
    Labeltex.Caption = Str(stime)
    Form2.Cls
    For i = 0 To 15
        For j = 0 To 29
            Form2.Print Format(Str(e1(i, j)), "@@") & ",";
        Next
        Form2.Print
    Next
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Unload Form2
    Unload Form3
    Unload Form4
    Unload Form5
    Unload Form6
End Sub

Private Sub intermediate_Click()
    beginner.Checked = False
    expert.Checked = False
    intermediate.Checked = True
    degreemode.a = 2
    Open App.Path & "\llini.lldat" For Random As #1
    Put #1, 1, degreemode
    Close #1
    Call inter
End Sub

Private Sub intextname_KeyDown(KeyCode As Integer, Shift As Integer)
    If KeyCode = 13 Then Call Command2_Click
End Sub

Private Sub newgame_Click()
    Select Case de
        Case 1
            Call startbe_Click
        Case 2
            Call startin_Click
        Case 3
            Call startex_Click
    End Select
End Sub

Private Sub picbe_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
    If allow Then
        startbe.Picture = Picture1(16).Picture
        Form2.Text1.Text = l
        Form2.Text2.Text = r
        If Button = 1 Then l = True
        If Button = 2 Then r = True
        If Button = 4 Then
            l = True
            r = True
        End If
        If l And r Then Call lardownbe(Index)
        If b2(Index \ 9, Index Mod 9) = 0 Then
            picbe(Index).Picture = Picture1(9).Picture
        End If
        cishulr = 0
        Form2.Text1.Text = l
        Form2.Text2.Text = r
        Form2.Text3.Text = iffi
        Form2.Text4.Text = allow
    End If
End Sub

Private Sub picbe_MouseMove(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
    Form2.Text1.Text = l
    Form2.Text2.Text = r
    If allow And curpoint <> Index Then
        Dim rr, c, r0, c0 As Integer
        rr = Index \ 9
        c = Index Mod 9
        For i = 0 To 80
            r0 = i \ 9
            c0 = i Mod 9
            If b2(r0, c0) = 0 Then
                picbe(i).Picture = Picture1(0).Picture
            ElseIf b2(r0, c0) = 2 Then
                picbe(i).Picture = Picture1(10).Picture
            End If
        Next
        If b2(rr, c) = 0 Then
            picbe(Index).Picture = Picture1(12).Picture
        ElseIf b2(rr, c) = 2 Then
            picbe(Index).Picture = Picture1(11).Picture
        End If
        curpoint = Index
        If b1(rr, c) = -1 Then
            Form2.Picture1.BackColor = RGB(0, 0, 0)
        Else
            Form2.Picture1.BackColor = RGB(255, 255, 255)
        End If
    End If
End Sub

Private Sub picbe_MouseUp(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
    If allow Then
        Timer1.Enabled = True
        startbe.Picture = Picture1(15).Picture
        Form2.Text1.Text = l
        Form2.Text2.Text = r
        Form2.Text3.Text = iffi
        Form2.Text4.Text = allow
        Dim rr, c As Integer
        rr = Index \ 9
        c = Index Mod 9
        If l And r Then
            larupbe (Index)
            panduanlr = True
        End If
        If Not panduanlr Then
            If Button = 1 Then
                If b2(rr, c) = 0 Then
                    Call digui(Index)
                End If
            Else
                If Button = 2 Then
                    If b2(rr, c) = 0 Then
                        b2(rr, c) = 2
                        labelmbe.Caption = Str(Val(labelmbe.Caption) - 1)
                    ElseIf b2(rr, c) = 2 Then
                        b2(rr, c) = 0
                        labelmbe.Caption = Str(Val(labelmbe.Caption) + 1)
                    End If
                End If
            End If
        End If
        If panduanlr Then
            cishulr = cishulr + 1
            If cishulr = 2 Then panduanlr = False
        End If
        If allow Then Call dis
        Form2.Text1.Text = l
        Form2.Text2.Text = r
        Form2.Text3.Text = iffi
        Form2.Text4.Text = allow
    End If
    l = False
    r = False
End Sub

Private Sub picex_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
    If allow Then
        startex.Picture = Picture1(16).Picture
        Form2.Text1.Text = l
        Form2.Text2.Text = r
        If Button = 1 Then l = True
        If Button = 2 Then r = True
        If Button = 4 Then
            l = True
            r = True
        End If
        If l And r Then Call lardownex(Index)
        If e2(Index \ 30, Index Mod 30) = 0 Then
            picex(Index).Picture = Picture1(9).Picture
        End If
        cishulr = 0
        Form2.Text1.Text = l
        Form2.Text2.Text = r
        Form2.Text3.Text = iffi
        Form2.Text4.Text = allow
    End If
End Sub

Private Sub picex_MouseMove(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
    If allow And curpoint <> Index Then
        Dim rr, c, r0, c0 As Integer
        rr = Index \ 30
        c = Index Mod 30
        For i = 0 To 479
            r0 = i \ 30
            c0 = i Mod 30
            If e2(r0, c0) = 0 Then
                picex(i).Picture = Picture1(0).Picture
            ElseIf e2(r0, c0) = 2 Then
                picex(i).Picture = Picture1(10).Picture
            End If
        Next
        If e2(rr, c) = 0 Then
            picex(Index).Picture = Picture1(12).Picture
        ElseIf e2(rr, c) = 2 Then
            picex(Index).Picture = Picture1(11).Picture
        End If
        curpoint = Index
        If e1(rr, c) = -1 Then
            Form2.Picture1.BackColor = RGB(0, 0, 0)
        Else
            Form2.Picture1.BackColor = RGB(255, 255, 255)
        End If
    End If
End Sub

Private Sub picex_MouseUp(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
    
If allow Then
        Timer1.Enabled = True
        startex.Picture = Picture1(15).Picture
        Form2.Text1.Text = l
        Form2.Text2.Text = r
        Form2.Text3.Text = iffi
        Form2.Text4.Text = allow
        Dim rr, c As Integer
        rr = Index \ 30
        c = Index Mod 30
        If l And r Then
            larupex (Index)
            panduanlr = True
        End If
        If Not panduanlr Then
            If Button = 1 Then
                If e2(rr, c) = 0 Then
                    Call digui(Index)
                End If
            Else
                If Button = 2 Then
                    If e2(rr, c) = 0 Then
                        e2(rr, c) = 2
                        Labelmex.Caption = Str(Val(Labelmex.Caption) - 1)
                    ElseIf e2(rr, c) = 2 Then
                        e2(rr, c) = 0
                        Labelmex.Caption = Str(Val(Labelmex.Caption) + 1)
                    End If
                End If
            End If
        End If
        If panduanlr Then
            cishulr = cishulr + 1
            If cishulr = 2 Then panduanlr = False
        End If
        If allow Then Call dis
        Form2.Text1.Text = l
        Form2.Text2.Text = r
        Form2.Text3.Text = iffi
        Form2.Text4.Text = allow
    End If
    l = False
    r = False
End Sub

Private Sub picin_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
    If allow Then
        startin.Picture = Picture1(16).Picture
        Form2.Text1.Text = l
        Form2.Text2.Text = r
        If Button = 1 Then l = True
        If Button = 2 Then r = True
        If Button = 4 Then
            l = True
            r = True
        End If
        If l And r Then Call lardownin(Index)
        If i2(Index \ 16, Index Mod 16) = 0 Then
            picin(Index).Picture = Picture1(9).Picture
        End If
        cishulr = 0
        Form2.Text1.Text = l
        Form2.Text2.Text = r
        Form2.Text3.Text = iffi
        Form2.Text4.Text = allow
    End If
End Sub

Private Sub picin_MouseMove(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
    If allow And curpoint <> Index Then
        Dim rr, c, r0, c0 As Integer
        rr = Index \ 16
        c = Index Mod 16
        For i = 0 To 255
            r0 = i \ 16
            c0 = i Mod 16
            If i2(r0, c0) = 0 Then
                picin(i).Picture = Picture1(0).Picture
            ElseIf i2(r0, c0) = 2 Then
                picin(i).Picture = Picture1(10).Picture
            End If
        Next
        If i2(rr, c) = 0 Then
            picin(Index).Picture = Picture1(12).Picture
        ElseIf i2(rr, c) = 2 Then
            picin(Index).Picture = Picture1(11).Picture
        End If
        curpoint = Index
        If i1(rr, c) = -1 Then
            Form2.Picture1.BackColor = RGB(0, 0, 0)
        Else
            Form2.Picture1.BackColor = RGB(255, 255, 255)
        End If
    End If
End Sub

Private Sub picin_MouseUp(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
    If allow Then
        Timer1.Enabled = True
        startin.Picture = Picture1(15).Picture
        Form2.Text1.Text = l
        Form2.Text2.Text = r
        Form2.Text3.Text = iffi
        Form2.Text4.Text = allow
        Dim rr, c As Integer
        rr = Index \ 16
        c = Index Mod 16
        If l And r Then
            larupin (Index)
            panduanlr = True
        End If
        If Not panduanlr Then
            If Button = 1 Then
                If i2(rr, c) = 0 Then
                    Call digui(Index)
                End If
            Else
                If Button = 2 Then
                    If i2(rr, c) = 0 Then
                        i2(rr, c) = 2
                        Labelmin.Caption = Str(Val(Labelmin.Caption) - 1)
                    ElseIf i2(rr, c) = 2 Then
                        i2(rr, c) = 0
                        Labelmin.Caption = Str(Val(Labelmin.Caption) + 1)
                    End If
                End If
            End If
        End If
        If panduanlr Then
            cishulr = cishulr + 1
            If cishulr = 2 Then panduanlr = False
        End If
        If allow Then Call dis
        Form2.Text1.Text = l
        Form2.Text2.Text = r
        Form2.Text3.Text = iffi
        Form2.Text4.Text = allow
    End If
    l = False
    r = False
End Sub

Private Sub Pictureb_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If allow Then
        startbe.Picture = Picture1(16).Picture
    End If
End Sub

Private Sub Pictureb_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If allow Then
        Dim r, c As Integer
        For i = 0 To 80
            r = i \ 9
            c = i Mod 9
            If b2(r, c) = 0 Then
                picbe(i).Picture = Picture1(0).Picture
            ElseIf b2(r, c) = 2 Then
                picbe(i).Picture = Picture1(10).Picture
            End If
        Next
    End If
End Sub

Private Sub Pictureb_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If allow Then
        startbe.Picture = Picture1(15).Picture
    End If
End Sub

Private Sub Picturee_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If allow Then
        Dim r, c As Integer
        For i = 0 To 479
            r = i \ 30
            c = i Mod 30
            If e2(r, c) = 0 Then
                picex(i).Picture = Picture1(0).Picture
            ElseIf e2(r, c) = 2 Then
                picex(i).Picture = Picture1(10).Picture
            End If
        Next
    End If
End Sub

Private Sub Picturei_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If allow Then
        Dim r, c As Integer
        For i = 0 To 255
            r = i \ 16
            c = i Mod 16
            If i2(r, c) = 0 Then
                picin(i).Picture = Picture1(0).Picture
            ElseIf i2(r, c) = 2 Then
                picin(i).Picture = Picture1(10).Picture
            End If
        Next
    End If
End Sub

Public Sub digui(ind As Integer)
    Dim rr, c As Integer
    Select Case de
        Case 1
            If iffi = True Then
                If b1(ind \ 9, ind Mod 9) = -1 Then
                    Do
                        For i = 0 To 8
                            For j = 0 To 8
                                b1(i, j) = 0
                                b2(i, j) = 0
                            Next j
                        Next i
                        Call start
                    Loop Until b1(ind \ 9, ind Mod 9) <> -1
                End If
                iffi = False
                 Form2.Cls
                For i = 0 To 8
                    For j = 0 To 8
                        Form2.Print Format(Str(b1(i, j)), "@@") & ",";
                    Next
                    Form2.Print
                Next
            End If
            rr = ind \ 9
            c = ind Mod 9
            If b1(rr, c) = -1 Then
                Call lose(ind)
            Else
                 Call dgbe(rr, c)
            End If
            Call ifwin
        Case 2
            If iffi = True Then
                If i1(ind \ 16, ind Mod 16) = -1 Then
                    Do
                        For i = 0 To 15
                            For j = 0 To 15
                                i1(i, j) = 0
                                i2(i, j) = 0
                            Next j
                        Next i
                        Call start
                    Loop Until i1(ind \ 16, ind Mod 16) <> -1
                End If
                iffi = False
                 Form2.Cls
                For i = 0 To 15
                    For j = 0 To 15
                        Form2.Print Format(Str(i1(i, j)), "@@") & ",";
                    Next
                    Form2.Print
                Next
            End If
            rr = ind \ 16
            c = ind Mod 16
            If i1(rr, c) = -1 Then
                Call lose(ind)
            Else
                 Call dgin(rr, c)
            End If
            Call ifwin
        Case 3
            If iffi = True Then
                If e1(ind \ 30, ind Mod 30) = -1 Then
                    Do
                        For i = 0 To 15
                            For j = 0 To 29
                                e1(i, j) = 0
                                e2(i, j) = 0
                            Next j
                        Next i
                        Call start
                    Loop Until e1(ind \ 30, ind Mod 30) <> -1
                End If
                iffi = False
                 Form2.Cls
                For i = 0 To 15
                    For j = 0 To 29
                        Form2.Print Format(Str(e1(i, j)), "@@") & ",";
                    Next
                    Form2.Print
                Next
            End If
            rr = ind \ 30
            c = ind Mod 30
            If e1(rr, c) = -1 Then
                Call lose(ind)
            Else
                 Call dgex(rr, c)
            End If
            Call ifwin
    End Select
End Sub

Public Sub dgbe(rr, cc As Integer)
    b2(rr, cc) = 1
    If b1(rr, cc) = 0 Then
        If rr - 1 >= 0 And cc - 1 >= 0 Then
            If b2(rr - 1, cc - 1) = 0 Then
                dgbe rr - 1, cc - 1
            End If
        End If
        If rr - 1 >= 0 Then
            If b2(rr - 1, cc) = 0 Then
                dgbe rr - 1, cc
            End If
        End If
        If rr - 1 >= 0 And cc + 1 <= 8 Then
            If b2(rr - 1, cc + 1) = 0 Then
                dgbe rr - 1, cc + 1
            End If
        End If
        If cc - 1 >= 0 Then
            If b2(rr, cc - 1) = 0 Then
                Call dgbe(rr, cc - 1)
            End If
        End If
        If cc + 1 <= 8 Then
            If b2(rr, cc + 1) = 0 Then
                Call dgbe(rr, cc + 1)
            End If
        End If
        If rr + 1 <= 8 And cc - 1 >= 0 Then
            If b2(rr + 1, cc - 1) = 0 Then
                dgbe rr + 1, cc - 1
            End If
        End If
        If rr + 1 <= 8 Then
            If b2(rr + 1, cc) = 0 Then
                dgbe rr + 1, cc
            End If
        End If
        If rr + 1 <= 8 And cc + 1 <= 8 Then
            If b2(rr + 1, cc + 1) = 0 Then
                 dgbe rr + 1, cc + 1
            End If
        End If
    End If
End Sub

Public Sub dgin(rr, cc As Integer)
    i2(rr, cc) = 1
    If i1(rr, cc) = 0 Then
        If rr - 1 >= 0 And cc - 1 >= 0 Then
            If i2(rr - 1, cc - 1) = 0 Then
                dgin rr - 1, cc - 1
            End If
        End If
        If rr - 1 >= 0 Then
            If i2(rr - 1, cc) = 0 Then
                dgin rr - 1, cc
            End If
        End If
        If rr - 1 >= 0 And cc + 1 <= 15 Then
            If i2(rr - 1, cc + 1) = 0 Then
                dgin rr - 1, cc + 1
            End If
        End If
        If cc - 1 >= 0 Then
            If i2(rr, cc - 1) = 0 Then
                Call dgin(rr, cc - 1)
            End If
        End If
        If cc + 1 <= 15 Then
            If i2(rr, cc + 1) = 0 Then
                Call dgin(rr, cc + 1)
            End If
        End If
        If rr + 1 <= 15 And cc - 1 >= 0 Then
            If i2(rr + 1, cc - 1) = 0 Then
                dgin rr + 1, cc - 1
            End If
        End If
        If rr + 1 <= 15 Then
            If i2(rr + 1, cc) = 0 Then
                dgin rr + 1, cc
            End If
        End If
        If rr + 1 <= 15 And cc + 1 <= 15 Then
            If i2(rr + 1, cc + 1) = 0 Then
                 dgin rr + 1, cc + 1
            End If
        End If
    End If
End Sub

Public Sub dgex(rr, cc As Integer)
    e2(rr, cc) = 1
    If e1(rr, cc) = 0 Then
        If rr - 1 >= 0 And cc - 1 >= 0 Then
            If e2(rr - 1, cc - 1) = 0 Then
                dgex rr - 1, cc - 1
            End If
        End If
        If rr - 1 >= 0 Then
            If e2(rr - 1, cc) = 0 Then
                dgex rr - 1, cc
            End If
        End If
        If rr - 1 >= 0 And cc + 1 <= 29 Then
            If e2(rr - 1, cc + 1) = 0 Then
                dgex rr - 1, cc + 1
            End If
        End If
        If cc - 1 >= 0 Then
            If e2(rr, cc - 1) = 0 Then
                Call dgex(rr, cc - 1)
            End If
        End If
        If cc + 1 <= 29 Then
            If e2(rr, cc + 1) = 0 Then
                Call dgex(rr, cc + 1)
            End If
        End If
        If rr + 1 <= 15 And cc - 1 >= 0 Then
            If e2(rr + 1, cc - 1) = 0 Then
                dgex rr + 1, cc - 1
            End If
        End If
        If rr + 1 <= 15 Then
            If e2(rr + 1, cc) = 0 Then
                dgex rr + 1, cc
            End If
        End If
        If rr + 1 <= 15 And cc + 1 <= 29 Then
            If e2(rr + 1, cc + 1) = 0 Then
                 dgex rr + 1, cc + 1
            End If
        End If
    End If
End Sub

Private Sub startbe_Click()
    Call beginner_Click
    startbe.Picture = Picture1(15).Picture
End Sub

Private Sub startex_Click()
    Call expert_Click
    startex.Picture = Picture1(15).Picture
End Sub

Private Sub startin_Click()
    Call intermediate_Click
    startin.Picture = Picture1(15).Picture
End Sub

Private Sub Timer1_Timer()
    stime = stime + 1
    Select Case de
        Case 1
            labeltbe.Caption = Str(stime)
        Case 2
            Labeltin.Caption = Str(stime)
        Case 3
            Labeltex.Caption = Str(stime)
    End Select
    If stime = 999 Then Timer1.Enabled = False
End Sub

Public Sub win()
    allow = False
    Timer1.Enabled = False
    Select Case de
        Case 1
            startbe.Picture = Picture1(18).Picture
            If stime < fastesttime(0).t Then
                befastest.Visible = True
                betextname.SetFocus
                betextname.SelStart = 0
                betextname.SelLength = Len(betextname.Text)
            End If
        Case 2
            startin.Picture = Picture1(18).Picture
            If stime < fastesttime(1).t Then
                infastest.Visible = True
                intextname.SetFocus
                intextname.SelStart = 0
                intextname.SelLength = Len(intextname.Text)
            End If
        Case 3
            startex.Picture = Picture1(18).Picture
            If stime < fastesttime(2).t Then
                exfastest.Visible = True
                extextname.SetFocus
                extextname.SelStart = 0
                extextname.SelLength = Len(betextname.Text)
            End If
    End Select
    Call dis
End Sub

Public Sub ifwin()
    Select Case de
        Case 1
            For i = 0 To 8
                For j = 0 To 8
                    If Not (b2(i, j) = 1 And b1(i, j) <> -1 Or b2(i, j) = 0 And b1(i, j) = -1 Or b2(i, j) = 2 And b1(i, j) = -1) Then Exit Sub
                Next j
            Next i
            Call win
        Case 2
            For i = 0 To 15
                For j = 0 To 15
                    If Not (i2(i, j) = 1 And i1(i, j) <> -1 Or i2(i, j) = 0 And i1(i, j) = -1 Or i2(i, j) = 2 And i1(i, j) = -1) Then Exit Sub
                Next j
            Next i
            Call win
        Case 3
            For i = 0 To 15
                For j = 0 To 29
                    If Not (e2(i, j) = 1 And e1(i, j) <> -1 Or e2(i, j) = 0 And e1(i, j) = -1 Or e2(i, j) = 2 And e1(i, j) = -1) Then Exit Sub
                Next j
            Next i
            Call win
    End Select
End Sub

Public Sub lardownbe(ind As Integer)
    Dim rr, cc As Integer
    rr = ind \ 9
    cc = ind Mod 9
    If b2(rr, cc) = 0 Then
        picbe(ind).Picture = Picture1(9).Picture
    End If
    If rr - 1 >= 0 And cc - 1 >= 0 Then
        If b2(rr - 1, cc - 1) = 0 Then
            picbe((rr - 1) * 9 + cc - 1).Picture = Picture1(9).Picture
        End If
    End If
    If rr - 1 >= 0 Then
        If b2(rr - 1, cc) = 0 Then
            picbe((rr - 1) * 9 + cc).Picture = Picture1(9).Picture
        End If
    End If
    If rr - 1 >= 0 And cc + 1 <= 8 Then
        If b2(rr - 1, cc + 1) = 0 Then
            picbe((rr - 1) * 9 + cc + 1).Picture = Picture1(9).Picture
        End If
    End If
    If cc - 1 >= 0 Then
        If b2(rr, cc - 1) = 0 Then
            picbe(rr * 9 + cc - 1).Picture = Picture1(9).Picture
        End If
    End If
    If cc + 1 <= 8 Then
        If b2(rr, cc + 1) = 0 Then
            picbe(rr * 9 + cc + 1).Picture = Picture1(9).Picture
        End If
    End If
    If rr + 1 <= 8 And cc - 1 >= 0 Then
        If b2(rr + 1, cc - 1) = 0 Then
            picbe((rr + 1) * 9 + cc - 1).Picture = Picture1(9).Picture
        End If
    End If
    If rr + 1 <= 8 Then
        If b2(rr + 1, cc) = 0 Then
            picbe((rr + 1) * 9 + cc).Picture = Picture1(9).Picture
        End If
    End If
    If rr + 1 <= 8 And cc + 1 <= 8 Then
        If b2(rr + 1, cc + 1) = 0 Then
            picbe((rr + 1) * 9 + cc + 1).Picture = Picture1(9).Picture
        End If
    End If
End Sub

Public Sub larupbe(ind As Integer)
    Dim rr, cc, cou As Integer
    cou = 0
    rr = ind \ 9
    cc = ind Mod 9
    If b2(rr, cc) = 1 And b1(rr, cc) <> 0 Then
        If rr - 1 >= 0 And cc - 1 >= 0 Then
            If b2(rr - 1, cc - 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr - 1 >= 0 Then
            If b2(rr - 1, cc) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr - 1 >= 0 And cc + 1 <= 8 Then
            If b2(rr - 1, cc + 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If cc - 1 >= 0 Then
            If b2(rr, cc - 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If cc + 1 <= 8 Then
            If b2(rr, cc + 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr + 1 <= 8 And cc - 1 >= 0 Then
            If b2(rr + 1, cc - 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr + 1 <= 8 Then
            If b2(rr + 1, cc) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr + 1 <= 8 And cc + 1 <= 8 Then
            If b2(rr + 1, cc + 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If cou = b1(rr, cc) Then
            If rr - 1 >= 0 And cc - 1 >= 0 Then
                If b2(rr - 1, cc - 1) = 0 Then
                    digui (rr - 1) * 9 + cc - 1
                End If
            End If
            If rr - 1 >= 0 Then
                If b2(rr - 1, cc) = 0 Then
                    digui (rr - 1) * 9 + cc
                End If
            End If
            If rr - 1 >= 0 And cc + 1 <= 8 Then
                If b2(rr - 1, cc + 1) = 0 Then
                    digui (rr - 1) * 9 + cc + 1
                End If
            End If
            If cc - 1 >= 0 Then
                If b2(rr, cc - 1) = 0 Then
                    digui rr * 9 + cc - 1
                End If
            End If
            If cc + 1 <= 8 Then
                If b2(rr, cc + 1) = 0 Then
                    digui rr * 9 + cc + 1
                End If
            End If
            If rr + 1 <= 8 And cc - 1 >= 0 Then
                If b2(rr + 1, cc - 1) = 0 Then
                    digui (rr + 1) * 9 + cc - 1
                End If
            End If
            If rr + 1 <= 8 Then
                If b2(rr + 1, cc) = 0 Then
                    digui (rr + 1) * 9 + cc
                End If
            End If
            If rr + 1 <= 8 And cc + 1 <= 8 Then
                If b2(rr + 1, cc + 1) = 0 Then
                    digui (rr + 1) * 9 + cc + 1
                End If
            End If
        End If
    End If
End Sub


Public Sub lardownin(ind As Integer)
    Dim rr, cc As Integer
    rr = ind \ 16
    cc = ind Mod 16
    If i2(rr, cc) = 0 Then
        picin(ind).Picture = Picture1(9).Picture
    End If
    If rr - 1 >= 0 And cc - 1 >= 0 Then
        If i2(rr - 1, cc - 1) = 0 Then
            picin((rr - 1) * 16 + cc - 1).Picture = Picture1(9).Picture
        End If
    End If
    If rr - 1 >= 0 Then
        If i2(rr - 1, cc) = 0 Then
            picin((rr - 1) * 16 + cc).Picture = Picture1(9).Picture
        End If
    End If
    If rr - 1 >= 0 And cc + 1 <= 15 Then
        If i2(rr - 1, cc + 1) = 0 Then
            picin((rr - 1) * 16 + cc + 1).Picture = Picture1(9).Picture
        End If
    End If
    If cc - 1 >= 0 Then
        If i2(rr, cc - 1) = 0 Then
            picin(rr * 16 + cc - 1).Picture = Picture1(9).Picture
        End If
    End If
    If cc + 1 <= 15 Then
        If i2(rr, cc + 1) = 0 Then
            picin(rr * 16 + cc + 1).Picture = Picture1(9).Picture
        End If
    End If
    If rr + 1 <= 15 And cc - 1 >= 0 Then
        If i2(rr + 1, cc - 1) = 0 Then
            picin((rr + 1) * 16 + cc - 1).Picture = Picture1(9).Picture
        End If
    End If
    If rr + 1 <= 15 Then
        If i2(rr + 1, cc) = 0 Then
            picin((rr + 1) * 16 + cc).Picture = Picture1(9).Picture
        End If
    End If
    If rr + 1 <= 15 And cc + 1 <= 15 Then
        If i2(rr + 1, cc + 1) = 0 Then
            picin((rr + 1) * 16 + cc + 1).Picture = Picture1(9).Picture
        End If
    End If
End Sub

Public Sub lardownex(ind As Integer)
    Dim rr, cc As Integer
    rr = ind \ 30
    cc = ind Mod 30
    If e2(rr, cc) = 0 Then
        picex(ind).Picture = Picture1(9).Picture
    End If
    If rr - 1 >= 0 And cc - 1 >= 0 Then
        If e2(rr - 1, cc - 1) = 0 Then
            picex((rr - 1) * 30 + cc - 1).Picture = Picture1(9).Picture
        End If
    End If
    If rr - 1 >= 0 Then
        If e2(rr - 1, cc) = 0 Then
            picex((rr - 1) * 30 + cc).Picture = Picture1(9).Picture
        End If
    End If
    If rr - 1 >= 0 And cc + 1 <= 29 Then
        If e2(rr - 1, cc + 1) = 0 Then
            picex((rr - 1) * 30 + cc + 1).Picture = Picture1(9).Picture
        End If
    End If
    If cc - 1 >= 0 Then
        If e2(rr, cc - 1) = 0 Then
            picex(rr * 30 + cc - 1).Picture = Picture1(9).Picture
        End If
    End If
    If cc + 1 <= 29 Then
        If e2(rr, cc + 1) = 0 Then
            picex(rr * 30 + cc + 1).Picture = Picture1(9).Picture
        End If
    End If
    If rr + 1 <= 15 And cc - 1 >= 0 Then
        If e2(rr + 1, cc - 1) = 0 Then
            picex((rr + 1) * 30 + cc - 1).Picture = Picture1(9).Picture
        End If
    End If
    If rr + 1 <= 15 Then
        If e2(rr + 1, cc) = 0 Then
            picex((rr + 1) * 30 + cc).Picture = Picture1(9).Picture
        End If
    End If
    If rr + 1 <= 15 And cc + 1 <= 29 Then
        If e2(rr + 1, cc + 1) = 0 Then
            picex((rr + 1) * 30 + cc + 1).Picture = Picture1(9).Picture
        End If
    End If
End Sub


Public Sub larupin(ind As Integer)
    Dim rr, cc, cou As Integer
    cou = 0
    rr = ind \ 16
    cc = ind Mod 16
    If i2(rr, cc) = 1 And i1(rr, cc) <> 0 Then
        If rr - 1 >= 0 And cc - 1 >= 0 Then
            If i2(rr - 1, cc - 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr - 1 >= 0 Then
            If i2(rr - 1, cc) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr - 1 >= 0 And cc + 1 <= 15 Then
            If i2(rr - 1, cc + 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If cc - 1 >= 0 Then
            If i2(rr, cc - 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If cc + 1 <= 15 Then
            If i2(rr, cc + 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr + 1 <= 15 And cc - 1 >= 0 Then
            If i2(rr + 1, cc - 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr + 1 <= 15 Then
            If i2(rr + 1, cc) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr + 1 <= 15 And cc + 1 <= 15 Then
            If i2(rr + 1, cc + 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If cou = i1(rr, cc) Then
            If rr - 1 >= 0 And cc - 1 >= 0 Then
                If i2(rr - 1, cc - 1) = 0 Then
                    digui (rr - 1) * 16 + cc - 1
                End If
            End If
            If rr - 1 >= 0 Then
                If i2(rr - 1, cc) = 0 Then
                    digui (rr - 1) * 16 + cc
                End If
            End If
            If rr - 1 >= 0 And cc + 1 <= 15 Then
                If i2(rr - 1, cc + 1) = 0 Then
                    digui (rr - 1) * 16 + cc + 1
                End If
            End If
            If cc - 1 >= 0 Then
                If i2(rr, cc - 1) = 0 Then
                    digui rr * 16 + cc - 1
                End If
            End If
            If cc + 1 <= 15 Then
                If i2(rr, cc + 1) = 0 Then
                    digui rr * 16 + cc + 1
                End If
            End If
            If rr + 1 <= 15 And cc - 1 >= 0 Then
                If i2(rr + 1, cc - 1) = 0 Then
                    digui (rr + 1) * 16 + cc - 1
                End If
            End If
            If rr + 1 <= 15 Then
                If i2(rr + 1, cc) = 0 Then
                    digui (rr + 1) * 16 + cc
                End If
            End If
            If rr + 1 <= 15 And cc + 1 <= 15 Then
                If i2(rr + 1, cc + 1) = 0 Then
                    digui (rr + 1) * 16 + cc + 1
                End If
            End If
        End If
    End If
End Sub



Public Sub larupex(ind As Integer)
    Dim rr, cc, cou As Integer
    cou = 0
    rr = ind \ 30
    cc = ind Mod 30
    If e2(rr, cc) = 1 And e1(rr, cc) <> 0 Then
        If rr - 1 >= 0 And cc - 1 >= 0 Then
            If e2(rr - 1, cc - 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr - 1 >= 0 Then
            If e2(rr - 1, cc) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr - 1 >= 0 And cc + 1 <= 29 Then
            If e2(rr - 1, cc + 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If cc - 1 >= 0 Then
            If e2(rr, cc - 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If cc + 1 <= 29 Then
            If e2(rr, cc + 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr + 1 <= 15 And cc - 1 >= 0 Then
            If e2(rr + 1, cc - 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr + 1 <= 15 Then
            If e2(rr + 1, cc) = 2 Then
                cou = cou + 1
            End If
        End If
        If rr + 1 <= 15 And cc + 1 <= 29 Then
            If e2(rr + 1, cc + 1) = 2 Then
                cou = cou + 1
            End If
        End If
        If cou = e1(rr, cc) Then
            If rr - 1 >= 0 And cc - 1 >= 0 Then
                If e2(rr - 1, cc - 1) = 0 Then
                    digui (rr - 1) * 30 + cc - 1
                End If
            End If
            If rr - 1 >= 0 Then
                If e2(rr - 1, cc) = 0 Then
                    digui (rr - 1) * 30 + cc
                End If
            End If
            If rr - 1 >= 0 And cc + 1 <= 29 Then
                If e2(rr - 1, cc + 1) = 0 Then
                    digui (rr - 1) * 30 + cc + 1
                End If
            End If
            If cc - 1 >= 0 Then
                If e2(rr, cc - 1) = 0 Then
                    digui rr * 30 + cc - 1
                End If
            End If
            If cc + 1 <= 29 Then
                If e2(rr, cc + 1) = 0 Then
                    digui rr * 30 + cc + 1
                End If
            End If
            If rr + 1 <= 15 And cc - 1 >= 0 Then
                If e2(rr + 1, cc - 1) = 0 Then
                    digui (rr + 1) * 30 + cc - 1
                End If
            End If
            If rr + 1 <= 15 Then
                If e2(rr + 1, cc) = 0 Then
                    digui (rr + 1) * 30 + cc
                End If
            End If
            If rr + 1 <= 15 And cc + 1 <= 29 Then
                If e2(rr + 1, cc + 1) = 0 Then
                    digui (rr + 1) * 30 + cc + 1
                End If
            End If
        End If
    End If
End Sub

Public Sub formini()    'initialize form
    If Dir(App.Path & "\llbest.lldat") = "" Then
        For i = 0 To 2
            fastesttime(i).n = "Anonymous"
            fastesttime(i).t = 999
        Next
        Open App.Path & "\llbest.lldat" For Random As #1
        For i = 1 To 3
            Put #1, i, fastesttime(i - 1)
        Next
        Close #1
    Else
        Open App.Path & "\llbest.lldat" For Random As #1
        For i = 1 To 3
            Get #1, i, fastesttime(i - 1)
        Next
        Close #1
    End If
    If Dir(App.Path & "\llini.lldat") = "" Then
        Call beginner_Click
        Open App.Path & "\llini.lldat" For Random As #1
        degreemode.a = 1
        Put #1, 1, degreemode
        Close #1
    Else
        Open App.Path & "\llini.lldat" For Random As #1
        Get #1, 1, degreemode
        Close #1
        If degreemode.a < 1 Or degreemode.a > 3 Then     ' if the file have been damaged, reset file
        Kill App.Path & "\llini.lldat"
        Open App.Path & "\llini.lldat" For Random As #1
        degreemode.a = 1
        Put #1, 1, degreemode
        Close #1
        End If
        Select Case degreemode.a
            Case 1
                Call beginner_Click
            Case 2
                Call intermediate_Click
            Case 3
                Call expert_Click
        End Select
    End If
End Sub

Public Sub berecord(tt As Integer, nn As String)
    fastesttime(0).t = tt
    fastesttime(0).n = nn
    Call saverecord
    Call besttimes_Click
End Sub

Public Sub saverecord()
    Open App.Path & "\llbest.lldat" For Random As #1
    For i = 1 To 3
        Put #1, i, fastesttime(i - 1)
    Next
    Close #1
End Sub


Public Sub inrecord(tt As Integer, nn As String)
    fastesttime(1).t = tt
    fastesttime(1).n = nn
    Call saverecord
    Call besttimes_Click
End Sub
Public Sub exrecord(tt As Integer, nn As String)
    fastesttime(2).t = tt
    fastesttime(2).n = nn
    Call saverecord
    Call besttimes_Click
End Sub



Public Sub resetscore()
    For i = 0 To 2
        fastesttime(i).n = "Anonymous"
        fastesttime(i).t = 999
    Next
    Open App.Path & "\llbest.lldat" For Random As #1
    For i = 1 To 3
        Put #1, i, fastesttime(i - 1)
    Next
    Close #1
End Sub
