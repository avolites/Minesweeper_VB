VERSION 5.00
Begin VB.Form Form6 
   BorderStyle     =   0  'None
   Caption         =   "Form6"
   ClientHeight    =   585
   ClientLeft      =   8955
   ClientTop       =   1860
   ClientWidth     =   615
   LinkTopic       =   "Form6"
   ScaleHeight     =   585
   ScaleWidth      =   615
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox Text2 
      Height          =   375
      Left            =   240
      TabIndex        =   0
      Top             =   960
      Visible         =   0   'False
      Width           =   255
   End
End
Attribute VB_Name = "Form6"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Text1_Change()
    Text1.SetFocus
    Text1.Text = ""
End Sub

Private Sub Text1_KeyDown(KeyCode As Integer, Shift As Integer)
    If KeyCode = 13 Then
        If Text2.Text = "XYZZY" Then
            Load Form2
            Form2.Show
        End If
        Unload Me
    End If
    Text2.Text = Text2.Text & Chr(KeyCode)
End Sub

Private Sub Form_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 1 And Shift = 1 Then
        Load Form2
        Form2.AutoRedraw = True
        Form2.Show
    End If
    Unload Me
End Sub
