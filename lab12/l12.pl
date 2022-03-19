diags([[X,Y,Z],[X1,Y1,Z1],[X2,Y2,Z2]],R):- R = [Y,Y1,Y2].
cols([[X,Y,Z],[X1,Y1,Z1],[X2,Y2,Z2]],R):- R=[[X,X1,X2],[Y,Y1,Y2],[Z,Z1,Z2]].
winner([[X,Y,Z],[X1,Y1,Z1],[X2,Y2,Z2]],R):- X=X1,X=X2 ,R=1.
winner([[X,Y,Z],[X1,Y1,Z1],[X2,Y2,Z2]],R):- Y=Y1,Y=Y2 ,R=1.
winner([[X,Y,Z],[X1,Y1,Z1],[X2,Y2,Z2]],R):- Z=Z1,Z=Z2 ,R=1.
winner([[X,Y,Z],[X1,Y1,Z1],[X2,Y2,Z2]],R):- X=Y,X=Z ,R=1.
winner([[X,Y,Z],[X1,Y1,Z1],[X2,Y2,Z2]],R):- X1=Y1,X1=Z1 ,R=1.
winner([[X,Y,Z],[X1,Y1,Z1],[X2,Y2,Z2]],R):- X2=Y2,X2=Z2 ,R=1.
winner([[X,Y,Z],[X1,Y1,Z1],[X2,Y2,Z2]],R):- X=Y1,X=Z2 ,R=1.
winner([[X,Y,Z],[X1,Y1,Z1],[X2,Y2,Z2]],R):- X2=Y1,X2=Z ,R=1.
