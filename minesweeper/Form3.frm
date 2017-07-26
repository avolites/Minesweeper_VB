VERSION 5.00
Begin VB.Form Form3 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Fastest Mine Sweepers"
   ClientHeight    =   2265
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   6180
   Icon            =   "Form3.frx":0000
   LinkTopic       =   "Form3"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2265
   ScaleWidth      =   6180
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command2 
      Caption         =   "OK"
      Height          =   375
      Left            =   3600
      TabIndex        =   13
      Top             =   1680
      Width           =   1815
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Reset Scores"
      Height          =   375
      Left            =   600
      TabIndex        =   12
      Top             =   1680
      Width           =   1815
   End
   Begin VB.Label Label12 
      Alignment       =   2  'Center
      Height          =   255
      Left            =   3600
      TabIndex        =   11
      Top             =   1080
      Width           =   2415
   End
   Begin VB.Label Label11 
      Alignment       =   2  'Center
      Height          =   255
      Left            =   3600
      TabIndex        =   10
      Top             =   720
      Width           =   2415
   End
   Begin VB.Label Label10 
      Alignment       =   2  'Center
      Height          =   255
      Left            =   3600
      TabIndex        =   9
      Top             =   360
      Width           =   2415
   End
   Begin VB.Label Label9 
      Caption         =   "seconds"
      Height          =   255
      Left            =   2400
      TabIndex        =   8
      Top             =   1080
      Width           =   735
   End
   Begin VB.Label Label8 
      Caption         =   "seconds"
      Height          =   255
      Left            =   2400
      TabIndex        =   7
      Top             =   720
      Width           =   735
   End
   Begin VB.Label Label7 
      Caption         =   "seconds"
      Height          =   255
      Left            =   2400
      TabIndex        =   6
      Top             =   360
      Width           =   735
   End
   Begin VB.Label Label6 
      Alignment       =   2  'Center
      Height          =   255
      Left            =   1440
      TabIndex        =   5
      Top             =   1080
      Width           =   855
   End
   Begin VB.Label Label5 
      Alignment       =   2  'Center
      Height          =   255
      Left            =   1440
      TabIndex        =   4
      Top             =   720
      Width           =   855
   End
   Begin VB.Label Label4 
      Alignment       =   2  'Center
      Height          =   255
      Left            =   1440
      TabIndex        =   3
      Top             =   360
      Width           =   855
   End
   Begin VB.Label Label3 
      Caption         =   "Expert :"
      Height          =   255
      Left            =   120
      TabIndex        =   2
      Top             =   1080
      Width           =   975
   End
   Begin VB.Label Label2 
      Caption         =   "Intermediate :"
      Height          =   255
      Left            =   120
      TabIndex        =   1
      Top             =   720
      Width           =   975
   End
   Begin VB.Label Label1 
      Caption         =   "Beginner :"
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   360
      Width           =   975
   End
End
Attribute VB_Name = "Form3"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Public Sub fa(bet As Integer, ben As String, intt As Integer, intn As String, ext As Integer, exn As String)
    Label4.Caption = Str(bet)
    Label5.Caption = Str(intt)
    Label6.Caption = Str(ext)
    Label10.Caption = ben
    Label11.Caption = intn
    Label12.Caption = exn
End Sub

Private Sub Command1_Click()
    Call Form1.resetscore
    Form3.Hide
    Call Form1.besttimes_Click
End Sub

Private Sub Command2_Click()
    Form3.Hide
End Sub
