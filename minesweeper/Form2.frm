VERSION 5.00
Begin VB.Form Form2 
   Caption         =   "Debug"
   ClientHeight    =   5235
   ClientLeft      =   60
   ClientTop       =   450
   ClientWidth     =   7545
   LinkTopic       =   "Form2"
   ScaleHeight     =   5235
   ScaleWidth      =   7545
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox Text11 
      Height          =   285
      Left            =   5880
      MaxLength       =   25
      TabIndex        =   15
      Top             =   3960
      Width           =   1575
   End
   Begin VB.TextBox Text10 
      Height          =   285
      Left            =   5880
      MaxLength       =   25
      TabIndex        =   14
      Top             =   3360
      Width           =   1575
   End
   Begin VB.TextBox Text9 
      Height          =   285
      Left            =   5880
      MaxLength       =   25
      TabIndex        =   13
      Top             =   2760
      Width           =   1575
   End
   Begin VB.TextBox Text8 
      Height          =   285
      Left            =   5880
      TabIndex        =   12
      Top             =   3720
      Width           =   615
   End
   Begin VB.TextBox Text7 
      Height          =   285
      Left            =   5880
      TabIndex        =   11
      Top             =   3120
      Width           =   615
   End
   Begin VB.TextBox Text6 
      Height          =   285
      Left            =   5880
      TabIndex        =   10
      Top             =   2520
      Width           =   615
   End
   Begin VB.PictureBox Picture1 
      Height          =   735
      Left            =   4800
      ScaleHeight     =   675
      ScaleWidth      =   795
      TabIndex        =   8
      Top             =   4200
      Width           =   855
   End
   Begin VB.TextBox Text4 
      Height          =   375
      Left            =   3600
      TabIndex        =   3
      Text            =   "Text4"
      Top             =   4320
      Width           =   975
   End
   Begin VB.TextBox Text3 
      Height          =   375
      Left            =   2400
      TabIndex        =   2
      Text            =   "Text3"
      Top             =   4320
      Width           =   1095
   End
   Begin VB.TextBox Text2 
      Height          =   375
      Left            =   1320
      TabIndex        =   1
      Text            =   "Text2"
      Top             =   4320
      Width           =   975
   End
   Begin VB.TextBox Text1 
      Height          =   375
      Left            =   240
      TabIndex        =   0
      Text            =   "Text1"
      Top             =   4320
      Width           =   975
   End
   Begin VB.Label Label5 
      Height          =   375
      Left            =   7080
      TabIndex        =   9
      Top             =   4800
      Width           =   375
   End
   Begin VB.Label Label4 
      Caption         =   "right"
      Height          =   255
      Left            =   1320
      TabIndex        =   7
      Top             =   4800
      Width           =   975
   End
   Begin VB.Label Label3 
      Caption         =   "left"
      Height          =   255
      Left            =   240
      TabIndex        =   6
      Top             =   4800
      Width           =   975
   End
   Begin VB.Label Label2 
      Caption         =   "allow"
      Height          =   255
      Left            =   3600
      TabIndex        =   5
      Top             =   4800
      Width           =   975
   End
   Begin VB.Label Label1 
      Caption         =   "iffi"
      Height          =   255
      Left            =   2400
      TabIndex        =   4
      Top             =   4800
      Width           =   975
   End
End
Attribute VB_Name = "Form2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Type besttime2
    t As Integer
    n As String * 25
End Type
Dim fastesttime2(2) As besttime2

Private Sub Label5_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 1 And Shift = 1 Then
        fastesttime2(0).t = Val(Text6.Text)
        fastesttime2(0).n = Text9.Text
        fastesttime2(1).t = Val(Text7.Text)
        fastesttime2(1).n = Text10.Text
        fastesttime2(2).t = Val(Text8.Text)
        fastesttime2(2).n = Text11.Text
        Open App.Path & "\llbest.lldat" For Random As #1
        For i = 1 To 3
            Put #1, i, fastesttime2(i - 1)
        Next
        Close #1
    End If
End Sub
