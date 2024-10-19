# 3DChess

This is a playable 3d chess simulation created using ray tracing. All chess rules excluding En Passant are implemented.


## Notes: 

Only runs in Linux due to use of the X11 library.

FPToolkit.c, M3d_matrix_tools.c, and xwd_tools_03.c are coded and maintained by Jeffery Ely.


## To Run:

Download all files, then run:
```
cc chess.c -lm -lX11
./a.out
```

## To Play:

Click on the piece you wish to move, then on the square you wish to move it to. To castle, move the king two spaces in the desired direction.
