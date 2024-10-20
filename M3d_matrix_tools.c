#include <stdio.h>
#include <math.h>


/*

 ( x')          (x)
 ( y')  =   M * (y)  
 ( z')          (z)
 ( 1 )          (1)

instead of (x',y',z',1) = (x,y,z,1) * M  

*/




int M3d_print_mat (double a[4][4])
{
  int r,c ;
  for (r = 0 ; r < 4 ; r++ ) {
      for (c = 0 ; c < 4 ; c++ ) {
           printf(" %12.4lf ",a[r][c]) ;
      }
      printf("\n") ;
  }

  return 1 ;
} 





int M3d_copy_mat (double a[4][4], double b[4][4])
// a = b
{
  int r,c ;
  for (r = 0 ; r < 4 ; r++ ) {
      for (c = 0 ; c < 4 ; c++ ) {
           a[r][c] = b[r][c] ;
      }
  }

  return 1 ;
} 





int M3d_make_identity (double a[4][4])
// a = I
{
  int r,c ;
  for (r = 0 ; r < 4 ; r++ ) {
      for (c = 0 ; c < 4 ; c++ ) {
           if (r == c) a[r][c] = 1.0 ;
               else    a[r][c] = 0.0 ;
      }
  }

  return 1 ;
} 





int M3d_make_translation (double a[4][4], double dx, double dy, double dz)
{
  M3d_make_identity(a) ;
  a[0][3] =  dx ;  a[1][3] = dy ;  a[2][3] = dz ;
  return 1 ;
}





int M3d_make_scaling (double a[4][4], double sx, double sy, double sz)
{
  M3d_make_identity(a) ;
  a[0][0] =  sx ;  a[1][1] = sy ;  a[2][2] = sz ;
  return 1 ;
}












int M3d_make_x_rotation_cs (double a[4][4], double cs, double sn)
// this one assumes cosine and sine are already known
{
  M3d_make_identity(a) ;

  a[1][1] =   cs ;  a[1][2] = -sn ;
  a[2][1] =   sn ;  a[2][2] =  cs ;

  return 1 ;
}



int M3d_make_y_rotation_cs (double a[4][4], double cs, double sn)
// this one assumes cosine and sine are already known
{
  M3d_make_identity(a) ;

  a[0][0] =   cs ;  a[0][2] =  sn ;
  a[2][0] =  -sn ;  a[2][2] =  cs ;

  return 1 ;
}


int M3d_make_z_rotation_cs (double a[4][4], double cs, double sn)
// this one assumes cosine and sine are already known
{
  M3d_make_identity(a) ;

  a[0][0] =   cs ;  a[0][1] = -sn ;
  a[1][0] =   sn ;  a[1][1] =  cs ;

  return 1 ;
}





int M3d_mat_mult (double res[4][4], double a[4][4], double b[4][4])
// res = a * b
// this is SAFE, i.e. the user can make a call such as 
// M3d_mat_mult(p,  p,q) or M3d_mat_mult(p,  q,p) or  M3d_mat_mult(p, p,p)
{
  double sum ;
  int k ;
  int r,c ;
  double tmp[4][4] ;

  for (r = 0 ; r < 4 ; r++ ) {
      for (c = 0 ; c < 4 ; c++ ) {
           sum = 0.0 ;
           for (k = 0 ; k < 4 ; k++) {
                 sum = sum + a[r][k]*b[k][c] ;
           }
           tmp[r][c] = sum ;
      }
  }


  M3d_copy_mat (res,tmp) ;

  return 1 ;
}





int M3d_mat_mult_pt (double P[3],   double m[4][4], double Q[3])
// P = m*Q
// SAFE, user may make a call like M3d_mat_mult_pt (W, m,W) ;
{
  double u,v,t ;

  u = m[0][0]*Q[0] + m[0][1]*Q[1] + m[0][2]*Q[2] + m[0][3] ;
  v = m[1][0]*Q[0] + m[1][1]*Q[1] + m[1][2]*Q[2] + m[1][3] ;
  t = m[2][0]*Q[0] + m[2][1]*Q[1] + m[2][2]*Q[2] + m[2][3] ;  

  P[0] = u ;
  P[1] = v ;
  P[2] = t ;
  
  return 1 ;
}





int M3d_mat_mult_points (double X[], double Y[], double Z[],
                         double m[4][4],
                         double x[], double y[], double z[], int numpoints)
// |X0 X1 X2 ...|       |x0 x1 x2 ...|
// |Y0 Y1 Y2 ...| = m * |y0 y1 y2 ...|
// |Z0 Z1 Z2 ...|       |z0 z1 z2 ...|  
// | 1  1  1 ...|       | 1  1  1 ...|

// SAFE, user may make a call like M3d_mat_mult_points (x,y,z,  m, x,y,z,  n) ;
{
  double u,v,t ;
  int i ;

  for (i = 0 ; i < numpoints ; i++) {
    u = m[0][0]*x[i] + m[0][1]*y[i] + m[0][2]*z[i] + m[0][3] ;
    v = m[1][0]*x[i] + m[1][1]*y[i] + m[1][2]*z[i] + m[1][3] ;
    t = m[2][0]*x[i] + m[2][1]*y[i] + m[2][2]*z[i] + m[2][3] ;    

    X[i] = u ;
    Y[i] = v ;
    Z[i] = t ;
  }

  return 1 ;
}






int M3d_x_product (double res[3], double a[3], double b[3])
// res = a x b  , cross product of two vectors
// SAFE: it is ok to make a call such as
// D3d_x_product (a,  a,b) or
// D3d_x_product (b,  a,b) or
// D3d_x_product (a,  a,a) 
{
    double r[3] ;
    int v ;
    
    r[0] = a[1]*b[2] - b[1]*a[2] ;
    r[1] = b[0]*a[2] - a[0]*b[2] ;
    r[2] = a[0]*b[1] - b[0]*a[1] ;

    res[0] = r[0] ;
    res[1] = r[1] ;
    res[2] = r[2] ;

    if ((res[0] == 0) && (res[1] == 0) && (res[2] == 0)) {
	v = 0 ;
    } else {
	v = 1 ;
    }

    return v ;
}






//===========================================================================
// For Advanced Graphics :
//===========================================================================
#define SX 0
#define SY 1
#define SZ 2

#define RX 3
#define RY 4
#define RZ 5

#define TX 6
#define TY 7
#define TZ 8

#define NX 9
#define NY 10
#define NZ 11

int M3d_make_movement_sequence_matrix(double v[4][4], double vi[4][4], int n, int mtype[], double mparam[]) {
	int i;
	double temp[4][4];
	M3d_make_identity(v);
	for (i = 0; i < n; i++) {
		if (mtype[i] == 0) {
			M3d_make_scaling(temp, mparam[i], 1, 1);
		}
		else if (mtype[i] == 1) {
			M3d_make_scaling(temp, 1, mparam[i], 1);
		}
		else if (mtype[i] == 2) {
			M3d_make_scaling(temp, 1, 1, mparam[i]);
		}
		else if (mtype[i] == 3) {
			M3d_make_x_rotation_cs(temp, cos((M_PI * mparam[i])/180), sin((M_PI * mparam[i])/180));
		}
		else if (mtype[i] == 4) {
			M3d_make_y_rotation_cs(temp, cos((M_PI * mparam[i])/180), sin((M_PI * mparam[i])/180));
		}
		else if (mtype[i] == 5) {
			M3d_make_z_rotation_cs(temp, cos((M_PI * mparam[i])/180), sin((M_PI * mparam[i])/180));
		}
		else if (mtype[i] == 6) {
			M3d_make_translation(temp, mparam[i], 0, 0);
		}
		else if (mtype[i] == 7) {
			M3d_make_translation(temp, 0, mparam[i], 0);
		}
		else if (mtype[i] == 8) {
			M3d_make_translation(temp, 0, 0, mparam[i]);
		}
		else if (mtype[i] == 9) {
			M3d_make_scaling(temp, -1, 1, 1);
		}
		else if (mtype[i] == 10) {
			M3d_make_scaling(temp, 1, -1, 1);
		}
		else if (mtype[i] == 11) {
			M3d_make_scaling(temp, 1, 1, -1);
		}
		M3d_mat_mult(v, temp, v);
	}
	
	M3d_make_identity(vi);
	for (i = n - 1; i >= 0; i--) {
		if (mtype[i] == 0) {
			M3d_make_scaling(temp, 1/mparam[i], 1, 1);
		}
		else if (mtype[i] == 1) {
			M3d_make_scaling(temp, 1, 1/mparam[i], 1);
		}
		else if (mtype[i] == 2) {
			M3d_make_scaling(temp, 1, 1, 1/mparam[i]);
		}
		else if (mtype[i] == 3) {
			M3d_make_x_rotation_cs(temp, cos((M_PI * mparam[i])/180), -sin((M_PI * mparam[i])/180));
		}
		else if (mtype[i] == 4) {
			M3d_make_y_rotation_cs(temp, cos((M_PI * mparam[i])/180), -sin((M_PI * mparam[i])/180));
		}
		else if (mtype[i] == 5) {
			M3d_make_z_rotation_cs(temp, cos((M_PI * mparam[i])/180), -sin((M_PI * mparam[i])/180));
		}
		else if (mtype[i] == 6) {
			M3d_make_translation(temp, -mparam[i], 0, 0);
		}
		else if (mtype[i] == 7) {
			M3d_make_translation(temp, 0, -mparam[i], 0);
		}
		else if (mtype[i] == 8) {
			M3d_make_translation(temp, 0, 0, -mparam[i]);
		}
		else if (mtype[i] == 9) {
			M3d_make_scaling(temp, -1, 1, 1);
		}
		else if (mtype[i] == 10) {
			M3d_make_scaling(temp, 1, -1, 1);
		}
		else if (mtype[i] == 11) {
			M3d_make_scaling(temp, 1, 1, -1);
		}
		M3d_mat_mult(vi, temp, vi);
		
	}
}



int M3d_view(double m[4][4], double mi[4][4], double eye[3], double coi[3], double up[3]) {
	int mtype[100], n;
	double mparam[100], a, b, c, p, h, q, upH, temp[4][4];
	
	a = coi[0] - eye[0];
	b = coi[1] - eye[1];
	c = coi[2] - eye[2];
	p = sqrt(pow(a, 2) + pow(c, 2)); // y rotation hypotenuse
	h = sqrt(pow(b, 2) + pow(p, 2)); // x rotation hypotenuse
	
	// Build m
	M3d_make_translation(m, -eye[0], -eye[1], -eye[2]);
	M3d_make_y_rotation_cs(temp, c/p, -a/p);
	M3d_mat_mult(m, temp, m);
	M3d_make_x_rotation_cs(temp, p/h, b/h);
	M3d_mat_mult(m, temp, m);
	// Find z rotation
	M3d_mat_mult_pt(up, m, up);
	upH = sqrt(pow(up[1], 2) + pow(up[0], 2));
	M3d_make_z_rotation_cs(temp, up[1]/upH, up[0]/upH);
	M3d_mat_mult(m, temp, m);
	
	// Build m inverse
	M3d_make_z_rotation_cs(mi, up[1]/upH, -up[0]/upH);
	M3d_make_x_rotation_cs(temp, p/h, -b/h);
	M3d_mat_mult(mi, temp, mi);
	M3d_make_y_rotation_cs(temp, c/p, a/p);
	M3d_mat_mult(mi, temp, mi);
	M3d_make_translation(temp, eye[0], eye[1], eye[2]);
	M3d_mat_mult(mi, temp, mi);
	
	
}






  
