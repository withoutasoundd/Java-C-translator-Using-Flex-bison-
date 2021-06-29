#!/usr/bin/env python3
import os
import subprocess
from tkinter import *
from pathlib import Path
from subprocess import check_output
import tkinter as tk
import re
import sys
        
class LineNumbers(tk.Text):
    def __init__(self, master, text_widget, **kwargs):
        super().__init__(master, **kwargs)

        self.text_widget = text_widget
        self.text_widget.bind('<KeyPress>', self.on_key_press)

        self.insert(1.0, '1')
        self.configure(state='disabled')

    def on_key_press(self, event=None):
        final_index = str(self.text_widget.index(tk.END))
        num_of_lines = final_index.split('.')[0]
        line_numbers_string = "\n".join(str(no + 1) for no in range(int(num_of_lines)))
        width = len(str(num_of_lines))

        self.configure(state='normal', width=width)
        self.delete(1.0, tk.END)
        self.insert(1.0, line_numbers_string)
        self.configure(state='disabled')

def presse():
	t2.delete('1.0', END)
	os.system("touch input.txt")
	os.system("touch output.txt")
	v1 = t1.get(1.0,END)
	f = open("input.txt", "w")
	f.write(v1)
	f.close()
	foo = check_output('./a.out <input.txt', shell=True).decode("utf-8") 
	k=re.sub(' +', ' ', foo)
	q = "\n".join([ll.rstrip() for ll in k.splitlines() if ll.strip()])
	t2.insert(1.0,q)
	
	
def supp():
	t1.delete('1.0', END)	
	t2.delete('1.0', END)
	
	
main=Tk()
main.attributes('-zoomed', True)
t1=Text(main,bg='black',fg='white',height=43,width=68)
t2=Text(main,bg='black',fg='white',height=43,width=67)
main.title("Traducteur Java ==> C++")
main.configure(bg='khaki4')

n1 = LineNumbers(main, t1, width=1,height=43,bg='white',fg='black')
B=Button(main,bg='Green',text='Traduire',fg='WHITE',height=3,width=22,command=presse)
S=Button(main,bg='Red',text='Supprimer',fg='WHITE',height=3,width=22,command=supp)
n1.grid(row=1,column=0,rowspan=3)
t1.grid(row=1,column=1,rowspan=3)
B.grid(row=1,column=2)
S.grid(row=2,column=2)
t2.grid(row=1,column=3,rowspan=4)


main.mainloop()


