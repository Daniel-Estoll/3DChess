#include "FPToolkit.c"
#include "xwd_tools_03.c"
#include "M3d_matrix_tools.c"


int window_width, window_height, window_square_size ;
double Half_window_size ;
double Half_angle_degrees ;
double Tan_half_angle ;



//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

// M controls the maximum number of objects :
#define M 300
#define MAX_WINDOW_WIDTH  2000
#define MAX_WINDOW_HEIGHT 1500
#define FURTHEST_DISTANCE 1e50
double obmat[M][4][4] ;
double obinv[M][4][4] ;
double color[M][3] ;
double V[4][4];
double Vi[4][4];
int groupID[M] ;
int obj_range[50][2];
int pieceID[50];
int num_pieces;
double piece_location[50][3];
int isPieceWhite[50];
int hasMoved[50];
double captureLocation[2][3];
double reflectivity[M];
int obtype[M];
double z_buffer[MAX_WINDOW_WIDTH][MAX_WINDOW_HEIGHT];
int num_objects;
int mapID[100];
int obtexture[M];
int isWhiteTurn;
int isInCheck[2];
int gameOver = 0;
int whiteWon = 0;

double EYE[3];
double light_in_eye_space[3] ;
double AMBIENT      = 0.2 ;
double MAX_DIFFUSE  = 0.4 ;
double SPECPOW      = 50 ;

int textureMap[10];



int Light_Model (double irgb[3],
                 double s[3],
                 double p[3],
                 double n[3],
                 double argb[3])
// s,p,n in eyespace

// irgb == inherent color of object (input to this function)
// s = location of start of ray (probably the eye)
// p = point on object (input to this function)
// n = normal to the object at p (input to this function)
// argb == actual color of object (output of this function)
// globals : AMBIENT, MAX_DIFFUSE, SPECPOW, light_in_eye_space[3]

// return 1 if successful, 0 if error
{

  double len ;
  double N[3] ; 
  len = sqrt(n[0]*n[0] + n[1]*n[1] + n[2]*n[2]) ;
  if (len == 0) return 0 ;
  N[0] = n[0]/len ;  N[1] = n[1]/len ;  N[2] = n[2]/len ;

  double E[3] ;
  E[0] = s[0] - p[0] ; 
  E[1] = s[1] - p[1] ; 
  E[2] = s[2] - p[2] ; 
  len = sqrt(E[0]*E[0] + E[1]*E[1] + E[2]*E[2]) ;
  if (len == 0) return 0 ;
  E[0] /= len ;  E[1] /= len ;  E[2] /= len ;
  double NdotE = N[0]*E[0] + N[1]*E[1] + N[2]*E[2] ;

  double L[3] ;
  L[0] = light_in_eye_space[0] - p[0] ; 
  L[1] = light_in_eye_space[1] - p[1] ; 
  L[2] = light_in_eye_space[2] - p[2] ; 
  len = sqrt(L[0]*L[0] + L[1]*L[1] + L[2]*L[2]) ;
  if (len == 0) return 0 ;
  L[0] /= len ;  L[1] /= len ;  L[2] /= len ;
  double NdotL = N[0]*L[0] + N[1]*L[1] + N[2]*L[2] ;





  double max_ambient_and_diffuse = AMBIENT + MAX_DIFFUSE ;
     // this needs to occur BEFORE you possibly jump to LLL below




  double intensity ;
  if (NdotL*NdotE < 0) {
    // eye and light are on opposite sides of polygon
    intensity = AMBIENT ; 
    goto LLL ;
  } else if ((NdotL < 0) && (NdotE < 0)) {
    // eye and light on same side but normal pointing "wrong" way
    N[0] *= (-1.0) ;    N[1] *= (-1.0) ;    N[2] *= (-1.0) ; 
    NdotL *= (-1.0) ;
    NdotE *= (-1.0) ;   // don't use NdotE below, probably should eliminate this
  }


  // ignore Blinn's variant
  double R[3] ; // Reflection vector of incoming light
  R[0] = 2*NdotL*N[0] - L[0] ;
  R[1] = 2*NdotL*N[1] - L[1] ;
  R[2] = 2*NdotL*N[2] - L[2] ;

  double EdotR = E[0]*R[0] + E[1]*R[1] + E[2]*R[2] ;

  double diffuse ;
  if (NdotL <= 0.0) { diffuse = 0.0 ; }
  else { diffuse = MAX_DIFFUSE*NdotL ; }

  double specular ;
  if (EdotR <= 0.0) { specular = 0.0 ; }
  else { specular = (1.0 - max_ambient_and_diffuse)*pow(EdotR,SPECPOW) ;}

  // printf("%lf %lf\n",diffuse,specular) ;
  intensity = AMBIENT + diffuse + specular ;



 LLL : ;

  double f,g ;
  if (intensity <= max_ambient_and_diffuse) {
    f = intensity / max_ambient_and_diffuse ;
    argb[0] = f * irgb[0] ;
    argb[1] = f * irgb[1] ;
    argb[2] = f * irgb[2] ;
  } else {
    f = (intensity - max_ambient_and_diffuse) / 
                           (1.0 - max_ambient_and_diffuse) ;
    g = 1.0 - f ;
    argb[0] = g * irgb[0] + f ;
    argb[1] = g * irgb[1] + f ;
    argb[2] = g * irgb[2] + f ;
  }

  return 1 ;
}





int quadratic(double a, double b, double c, double result[2])
// return number of solutions
{
  int n ;
  double d,ta ;

  d = b*b - 4*a*c ;

  if (d < 0) {
     n = 0 ;  
  } else if (d == 0) {
     ta = 2*a ;
     result[0] = -b / ta ;
     n = 1 ;
  } else {
     d = sqrt(d) ;
     ta = 2*a ;
     result[0] = (-b + d) / ta ;
     result[1] = (-b - d) / ta ;
     n = 2 ;
  }

  return n ;
}// end quadratic




int normal_in_object_space(int obnum, double x, double y, double z, double normal[3]) {
	if (obtype[obnum] == 0) {
		normal[0] = 0;
		normal[1] = 1;
		normal[2] = 0;
	} else if (obtype[obnum] == 1) {
		normal[0] = 2 * x;
		normal[1] = 2 * y;
		normal[2] = 2 * z;
	} else if (obtype[obnum] == 2) {
		normal[0] = 2 * x;
		normal[1] = 0;
		normal[2] = 2 * z;
	} else if (obtype[obnum] == 3) {
		normal[0] = (2 * x) / (y * y);
		normal[1] = (-2 * (x * x + z * z)) / (y * y * y);
		normal[2] = (2 * z) / (y * y);
	} else if (obtype[obnum] == 4) {
		normal[0] = (2 * x) / y;
		normal[1] = -(x * x + z * z) / (y * y);
		normal[2] = (2 * z) / y;
	}

}


// Object Numbers: 0 = Plane, 1 = Sphere, 2 = Cylinder, 3 = Cone, 4 = Elliptic Paraboloid
// Plane Equation: 0x + y + 0z = 0
// Sphere Equation = x^2 + y^2 + z^2 - 1 = 0
// Cylinder Equation = x^2 + z^2 - 1 = 0
// Cone Equation = (x^2 + z^2)/y^2 = 0
// Elliptic Paraboloid Equation = (x^2 + z^2)/y = 0
int intersect_ray_with_objects(int obnum, double a[3], double c[3], double res[1]) {
	double xT, yT, zT, A, B, C, r[2], intersect[2];
	
	// Variables for T multipliers
	xT = c[0] - a[0];
	yT = c[1] - a[1];
	zT = c[2] - a[2];
	
	// Plane
	if (obtype[obnum] == 0) {
		r[0] = -a[1] / yT;
		intersect[0] = a[0] + xT * r[0];
		intersect[1] = a[2] + zT * r[0];
		if (r[0] < 0) {return 0;}
		if (intersect[0] > -1 && intersect[0] < 1 && intersect[1] > -1 && intersect[1] < 1) {
			res[0] = r[0];
			return 1;
		}
	} 
	// Sphere
	else if (obtype[obnum] == 1) {
		// Quadratic Equation coefficients
		A = (xT * xT) + (yT * yT) + (zT * zT);
		B = (2 * a[0] * xT) + (2 * a[1] * yT) + (2 * a[2] * zT);
		C = (a[0] * a[0]) + (a[1] * a[1]) + (a[2] * a[2]) - 1;
		
		if (quadratic(A, B, C, r) != 0) {
			if (isnan(r[0]) && isnan(r[1])) {return 0;}
			if (r[0] < 0 && r[1] < 0) {return 0;}
			else if (r[0] >= 0 && r[1] < 0) {
				res[0] = r[0];
			} else if (r[0] < 0 && r[1] >= 0) {
				res[0] = r[1];
			} else if (r[0] < r[1]) {
				res[0] = r[0];
			}else {
				res[0] = r[1];
			}
			return 1;
		}
	}
	// Cylinder
	 else if (obtype[obnum] == 2) {
		// Quadratic Equation coefficients
		A = (xT * xT) +  (zT * zT);
		B = (2 * a[0] * xT) +  (2 * a[2] * zT);
		C = (a[0] * a[0]) +  (a[2] * a[2]) - 1;
		
		if (quadratic(A, B, C, r) != 0) {
			intersect[0] = a[1] + yT * r[0];
			intersect[1] = a[1] + yT * r[1];
			if (isnan(r[0]) && isnan(r[1])) {return 0;}
			if ((intersect[0] > 1 || intersect[0] < 0) && (intersect[1] > 1 || intersect[1] < 0)) {return 0;}
			if (r[0] < 0 && r[1] < 0) {return 0;}
			else if (r[0] >= 0 && r[1] < 0 && intersect[0] >= 0 && intersect[0] <= 1) {
				res[0] = r[0];
			} else if (r[0] < 0 && r[1] >= 0 && intersect[1] >= 0 && intersect[1] <= 1) {
				res[0] = r[1];
			} else if (r[0] <= r[1] && intersect[0] >= 0 && intersect[0] <= 1) {
				res[0] = r[0];
			} else if (intersect[1] >= 0 && intersect[1] <= 1){
				res[0] = r[1];
			} else if (intersect[0] >= 0 && intersect[0] <= 1){
				res[0] = r[0];
			} else {return 0;}
			return 1;
		}
	}
	// Cone
	else if (obtype[obnum] == 3) {
		// Quadratic Equation coefficients
		A = (xT * xT) - (yT * yT) + (zT * zT);
		B = (2 * a[0] * xT) - (2 * a[1] * yT) + (2 * a[2] * zT);
		C = (a[0] * a[0]) - (a[1] * a[1]) + (a[2] * a[2]);
		
		if (quadratic(A, B, C, r) != 0) {
			intersect[0] = a[1] + yT * r[0];
			intersect[1] = a[1] + yT * r[1];
			if (isnan(r[0]) && isnan(r[1])) {return 0;}
			if ((intersect[0] > 1 || intersect[0] < 0) && (intersect[1] > 1 || intersect[1] < 0)) {return 0;}
			if (r[0] < 0 && r[1] < 0) {return 0;}
			else if (r[0] >= 0 && r[1] < 0 && intersect[0] >= 0 && intersect[0] <= 1) {
				res[0] = r[0];
			} else if (r[0] < 0 && r[1] >= 0 && intersect[1] >= 0 && intersect[1] <= 1) {
				res[0] = r[1];
			} else if (r[0] < r[1] && intersect[0] >= 0 && intersect[0] <= 1) {
				res[0] = r[0];
			} else if (intersect[1] >= 0 && intersect[1] <= 1){
				res[0] = r[1];
			} else if (intersect[0] >= 0 && intersect[0] <= 1){
				res[0] = r[0];
			} else {return 0;}
			return 1;
		}
	}
	else if (obtype[obnum] == 4) {
		// Quadratic Equation coefficients
		A = (xT * xT) + (zT * zT);
		B = (2 * a[0] * xT) - (yT) + (2 * a[2] * zT);
		C = (a[0] * a[0]) - (a[1]) + (a[2] * a[2]);
		
		if (quadratic(A, B, C, r) != 0) {
			intersect[0] = a[1] + yT * r[0];
			intersect[1] = a[1] + yT * r[1];
			if (isnan(r[0]) && isnan(r[1])) {return 0;}
			if ((intersect[0] > 1 || intersect[0] < 0) && (intersect[1] > 1 || intersect[1] < 0)) {return 0;}
			if (r[0] < 0 && r[1] < 0) {return 0;}
			else if (r[0] >= 0 && r[1] < 0 && intersect[0] >= 0 && intersect[0] <= 1) {
				res[0] = r[0];
			} else if (r[0] < 0 && r[1] >= 0 && intersect[1] >= 0 && intersect[1] <= 1) {
				res[0] = r[1];
			} else if (r[0] < r[1] && intersect[0] >= 0 && intersect[0] <= 1) {
				res[0] = r[0];
			} else if (intersect[1] >= 0 && intersect[1] <= 1){
				res[0] = r[1];
			} else if (intersect[0] >= 0 && intersect[0] <= 1){
				res[0] = r[0];
			} else {return 0;}
			return 1;
		}
	}

	return 0;
}




double dot_product(double a[3], double c[3]) {
	double r = 0;
	r += a[0] * c[0];
	r += a[1] * c[1];
	r += a[2] * c[2];
	return r;
}




int reflection_vector(double Nu[3], double Vu[3], double Ru[3]) {
	double dot = dot_product(Nu, Vu);
	Ru[0] = (2 * dot) * Nu[0] - Vu[0];
	Ru[1] = (2 * dot) * Nu[1] - Vu[1];
	Ru[2] = (2 * dot) * Nu[2] - Vu[2];
	return 1;
}




int get_pixel_texture(int obnum, double pt[3], double rgb[3]) {
	double u, v, r;
	int map = obtexture[obnum], d[2], mapX, mapY, e;
	if (map == 0) {
		rgb[0] = .3; rgb[1] = .3; rgb[2] = .3; return 1;
	}
	if (map == -1) {return 0;}
	if (obtype[obnum] == 0) {
		u = (pt[0] * -1) / 2 + 0.5;
		v = pt[2] / 2 + 0.5;
	}else if (obtype[obnum] == 1) {
		r = sqrt(pt[0] * pt[0] + pt[2] * pt[2]);
		u = atan2(pt[2], pt[0])/(2 * M_PI) + 0.5;
		v = atan2(pt[1], r)/(M_PI) + 0.5;
	} else {
		u = atan2(pt[2], pt[0])/(2 * M_PI) + 0.5;
		v = pt[1];
	}
	e = get_xwd_map_dimensions(mapID[map], d) ;
	if (e == -1) { printf("No map found\n") ;  exit(0) ; }
	mapX = u * d[0];
	mapY = v * d[1];
	get_xwd_map_color(mapID[map], mapX, mapY, rgb);
	return 1;
}




int ray(double argb[3], double start[3], double end[3], double board_pt[3]) {
	int i, j, numInt, closestObj, objects_intersected;	
	double A, B, C, res[1], closestD, intPt[3], objPt[3], intersectD, normalLen, ptObjSpcC[3], closest[3], objStart[3], objEnd[3], normal[3], normalOb[3], Vu[3], Ru[3], vuLen, nuE[3], ptLift[3], reflectRGB[3], irgb[3], objects_to_intersect[1000];
		
	
	closestD = FURTHEST_DISTANCE;
	numInt = 0;
	
	// Add board intersection
	objects_intersected = 0;
	objects_to_intersect[objects_intersected] = 0;
	objects_intersected++;
	// For each piece
	for (i = 0; i < num_pieces; i++) {
		// move start and end to objspace
		M3d_mat_mult_pt(objStart, obinv[obj_range[i][0]], start);
		M3d_mat_mult_pt(objEnd, obinv[obj_range[i][0]], end);
		//Check whether the ray intersects with it's bounding box
		if (intersect_ray_with_objects(obj_range[i][0], objStart, objEnd, res) != 0) {
			// Add the objects within that piece to be checked
			for (j = obj_range[i][0] + 1; j < obj_range[i][1]; j++) {
				objects_to_intersect[objects_intersected] = j;
				objects_intersected++;
			}
		}
	}
	
	for (j = 0; j < objects_intersected; j++) {
		i = objects_to_intersect[j];
		// move start and end to objspace
		M3d_mat_mult_pt(objStart, obinv[i], start);
		M3d_mat_mult_pt(objEnd, obinv[i], end);
		
		if (intersect_ray_with_objects(i, objStart, objEnd, res) == 0) { continue;}
	
		numInt++;
		intPt[0] = objStart[0] + (objEnd[0] - objStart[0]) * res[0];
		intPt[1] = objStart[1] + (objEnd[1] - objStart[1]) * res[0];
		intPt[2] = objStart[2] + (objEnd[2] - objStart[2]) * res[0];
		
	
		objPt[0] = intPt[0];
		objPt[1] = intPt[1];
		objPt[2] = intPt[2];
		
		
		if (i == 0) {
			board_pt[0] = objPt[0];
			board_pt[1] = objPt[1];
			board_pt[2] = objPt[2];
		}
		
		
		M3d_mat_mult_pt(intPt, obmat[i], intPt);
		
		// Update closest point
		intersectD = sqrt(intPt[0] * intPt[0] + intPt[1] * intPt[1] + intPt[2] * intPt[2]);
		if (intersectD < closestD) {
			closestD = intersectD;
			closest[0] = intPt[0];
			closest[1] = intPt[1];
			closest[2] = intPt[2]; 
			ptObjSpcC[0] = objPt[0];
			ptObjSpcC[1] = objPt[1];
			ptObjSpcC[2] = objPt[2];
			closestObj = i;
			if (get_pixel_texture(i, objPt, irgb) == 0) {
				irgb[0] = color[i][0];
				irgb[1] = color[i][1];
				irgb[2] = color[i][2];
			}
		}
	}
	
	
	if (numInt != 0) {
		// Find Normal vector
		normal_in_object_space(closestObj, ptObjSpcC[0], ptObjSpcC[1], ptObjSpcC[2], normalOb);
	  normal[0] = (normalOb[0] * obinv[closestObj][0][0]) + (normalOb[1] * obinv[closestObj][1][0]) + (normalOb[2] * obinv[closestObj][2][0]); // normal vector X 
		normal[1] = (normalOb[0] * obinv[closestObj][0][1]) + (normalOb[1] * obinv[closestObj][1][1]) + (normalOb[2] * obinv[closestObj][2][1]); // normal vector Y 
		normal[2] = (normalOb[0] * obinv[closestObj][0][2]) + (normalOb[1] * obinv[closestObj][1][2]) + (normalOb[2] * obinv[closestObj][2][2]); // normal vector Z 
		normalLen = sqrt(normal[0] * normal[0] + normal[1] * normal[1] + normal[2] * normal[2]);
		normal[0] = (normal[0] / normalLen); // Create vector with length 1
		normal[1] = (normal[1] / normalLen); // Create vector with length 1
		normal[2] = (normal[2] / normalLen); // Create vector with length 1
		// Prepare vector for reflection
		Vu[0] = closest[0] - start[0];
		Vu[1] = closest[1] - start[1];
		Vu[2] = closest[2] - start[2];
		vuLen = sqrt(Vu[0] * Vu[0] + Vu[1] * Vu[1] + Vu[2] * Vu[2]);
		Vu[0] = -(Vu[0]/vuLen);
		Vu[1] = -(Vu[1]/vuLen);
		Vu[2] = -(Vu[2] /vuLen);
		reflection_vector(normal, Vu, Ru);
		// Prepare for next ray
		ptLift[0] = closest[0] + Ru[0] * .001;
		ptLift[1] = closest[1] + Ru[1] * .001;
		ptLift[2] = closest[2] + Ru[2] * .001;
		nuE[0] = closest[0] + Ru[0];
		nuE[1] = closest[1] + Ru[1];
		nuE[2] = closest[2] + Ru[2];
		
		Light_Model(irgb, start, closest, normal, argb);
		
		return closestObj;
	}
	return -1;


} // end ray





void draw() {
	int counter, xP, yP;
	double x, y, screenX, screenY, rgb[3], pixel[3], board_pt[3];
	
	counter = 0; xP = 0; yP = 0;
	for (x = -Tan_half_angle; x < Tan_half_angle; x += (2 * Tan_half_angle) / window_width) {
		yP = 0;
		for (y = -Tan_half_angle; y < Tan_half_angle; y += (2 * Tan_half_angle) / window_height) {
			pixel[0] = x; pixel[1] = y; pixel[2] = 1;
			if (ray(rgb, EYE, pixel, board_pt) == -1) {
				rgb[0] = 0; rgb[1] = 0; rgb[2] = 0;
			}
			G_rgb(rgb[0], rgb[1], rgb[2]);
			
			G_point(xP, yP);
			//printf("%d, %d\n", xP, yP);
			yP++;
		}
		xP++;
	}
}




///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////


void create_pawn(double x, double y, double z, int isWhite) {
	double  Tvlist[30], m[4][4], mi[4][4];
	int Ttypelist[30], Tn;
	
	pieceID[num_pieces] = 1;
	
	obj_range[num_pieces][0] = num_objects;
	
	isPieceWhite[num_pieces] = isWhite;
	
	piece_location[num_pieces][0] = x;
	piece_location[num_pieces][1] = y;
	piece_location[num_pieces][2] = z;
	
	// Bounding Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 2;
	
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  7   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  2  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  2  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 7  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	//Bottom Sphere 1
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .25   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + .125  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  //Bottom Sphere 2
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .25   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + .25  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	// Bottom Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  1   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 1.5   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Central Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  4.5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.2   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.2   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 5   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;


  // Central down-pointing Cone
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 3;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 4  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Central up-pointing Cone
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 3;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 5  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Top Sphere
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .8   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  .8  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  .8  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 5.5  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  obj_range[num_pieces][1] = num_objects;
}
// End create_pawn



void create_rook (double x, double y, double z, int isWhite) {
	double  Tvlist[30], m[4][4], mi[4][4];
	int Ttypelist[30], Tn;
	
	pieceID[num_pieces] = 2;
	
	obj_range[num_pieces][0] = num_objects;
	
	isPieceWhite[num_pieces] = isWhite;
	
	piece_location[num_pieces][0] = x;
	piece_location[num_pieces][1] = y;
	piece_location[num_pieces][2] = z;
	
	
	// Bounding Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 2;
	
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  7   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 7  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	//Bottom Sphere 1
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .25   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + .125  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  //Bottom Sphere 2
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .25   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + .25  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	// Bottom Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  1   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 1.5   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	// Central Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  5.5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.2   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.2   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 6   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Central down-pointing Cone
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 3;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  4   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 2  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Upper up-pointing Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .3   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.4; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.4; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 6.3  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Upper Cylinder
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 2;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
  Tn = 0 ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  1   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.4; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.4; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 5.5  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
  
  M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  obj_range[num_pieces][1] = num_objects;
}
// End create_rook



void create_knight(double x, double y, double z, int isWhite) {
	double  Tvlist[30], m[4][4], mi[4][4];
	int Ttypelist[30], Tn, r = 0;
	
	pieceID[num_pieces] = 3;
	
	obj_range[num_pieces][0] = num_objects;
	
	isPieceWhite[num_pieces] = isWhite;
	
	piece_location[num_pieces][0] = x;
	piece_location[num_pieces][1] = y;
	piece_location[num_pieces][2] = z;
	
	if (!isWhite) {
		r = 90;
	}
	
	// Bounding Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 2;
	
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  8   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 8  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	//Bottom Sphere 1
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .25   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + .125  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  //Bottom Sphere 2
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .25   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + .25  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	// Bottom Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  1   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 1.5   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Lower Central Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  2.5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.6   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.6   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 3.5   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Central Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  4   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.4   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.3   ; Tn++ ;
	Ttypelist[Tn] = RX ; Tvlist[Tn] =  -12   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  5.15   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  -1  ; Tn++ ;
  Ttypelist[Tn] = RY ; Tvlist[Tn] =  45 + r  ; Tn++ ;
  Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Upper Central Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  3.3   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.1   ; Tn++ ;
	Ttypelist[Tn] = RX ; Tvlist[Tn] =  15   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  7.2   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  .3  ; Tn++ ;
  Ttypelist[Tn] = RY ; Tvlist[Tn] =  45 + r  ; Tn++ ;
  Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Upper Sphere
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .9   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  .6  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  .9  ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  6.6  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  .1  ; Tn++ ;
  Ttypelist[Tn] = RY ; Tvlist[Tn] =  45 + r  ; Tn++ ;
  Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
  
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Upper Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  1.5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  .6   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  .8   ; Tn++ ;
	Ttypelist[Tn] = RX ; Tvlist[Tn] =  115   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  5.9   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
  Ttypelist[Tn] = RY ; Tvlist[Tn] =  45 + r   ; Tn++ ;
  Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Top left cone
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 3;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .3   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  .1   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  .1   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  .2   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  7.75   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  .2  ; Tn++ ;
  Ttypelist[Tn] = RY ; Tvlist[Tn] =  45 + r  ; Tn++ ;
  Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Top left cone
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 3;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .3   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  .1   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  .1   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  -.2    ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  7.75   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  .2  ; Tn++ ;
  Ttypelist[Tn] = RY ; Tvlist[Tn] =  45 + r  ; Tn++ ;
  Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  obj_range[num_pieces][1] = num_objects;
}
// End create_knight



void create_bishop (double x, double y, double z, int isWhite) {
	double  Tvlist[30], m[4][4], mi[4][4];
	int Ttypelist[30], Tn;
	
	pieceID[num_pieces] = 4;
	
	obj_range[num_pieces][0] = num_objects;
	
	isPieceWhite[num_pieces] = isWhite;
	
	piece_location[num_pieces][0] = x;
	piece_location[num_pieces][1] = y;
	piece_location[num_pieces][2] = z;
	
	// Bounding Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 2;
	
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  8   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 8  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	//Bottom Sphere 1
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .25   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + .125  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  //Bottom Sphere 2
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .25   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + .25  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
	// Bottom Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  1   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 1.5   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	// Central Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  6.5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.2   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.2   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 7   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	// Central down-pointing Cone
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 3;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 5.5  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Central up-pointing Cone
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 3;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 6.5  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Upper down-pointing Elliptic Paraboiloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  1   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.1; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.1; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 6  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Upper up-pointing Cone
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 3;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  1.5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.1; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.1; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 8.5  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Top Sphere
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .2   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  .2  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  .2  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 8.5  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  obj_range[num_pieces][1] = num_objects;
}
// End create_bishop




void create_queen (double x, double y, double z, int isWhite) {
	double  Tvlist[30], m[4][4], mi[4][4];
	int Ttypelist[30], Tn;
	
	pieceID[num_pieces] = 5;
	
	obj_range[num_pieces][0] = num_objects;
	
	isPieceWhite[num_pieces] = isWhite;
	
	piece_location[num_pieces][0] = x;
	piece_location[num_pieces][1] = y;
	piece_location[num_pieces][2] = z;
	
	// Bounding Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 2;
	
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  10   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 10  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	//Bottom Sphere 1
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .25   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + .125  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  //Bottom Sphere 2
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .25   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + .25  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	// Bottom Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  1   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 1.5   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Central up-pointing Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  8   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.2   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.2   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 9   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Central Down-pointing Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  3   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 6   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Central Cylinder
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 2;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  4   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  .8   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  .8   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 3.75   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	// Central down-pointing Cone
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 3;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 6.5  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Central up-pointing Cone
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 3;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 7.5  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Upper Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .4   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 9.4  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Top Sphere
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .2   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  .2  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  .2  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 9.55  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;

	obj_range[num_pieces][1] = num_objects;
  
}
// End create_queen



void create_king (double x, double y, double z, int isWhite) {
	double  Tvlist[30], m[4][4], mi[4][4];
	int Ttypelist[30], Tn;
	
	pieceID[num_pieces] = 6;
	
	obj_range[num_pieces][0] = num_objects;
	
	isPieceWhite[num_pieces] = isWhite;
	
	piece_location[num_pieces][0] = x;
	piece_location[num_pieces][1] = y;
	piece_location[num_pieces][2] = z;
	
	// Bounding Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 2;
	
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  10.5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 10.5  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	//Bottom Sphere 1
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .25   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + .125  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  //Bottom Sphere 2
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .25   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75  ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + .25  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	// Bottom Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  1   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.75   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.75   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 1.5   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Central up-pointing Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  8   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.2   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.2   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 9   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Central Down-pointing Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  3   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 6   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Central Cylinder
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 2;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  4   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  .8   ; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  .8   ; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 3.75   ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
	
	// Central down-pointing Cone
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 3;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 6.5  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Central up-pointing Cone
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 3;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1.3; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 7.5  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Upper Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 4;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .6   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  1; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  1; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 9.6  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Top Elliptic Paraboloid
	groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .2   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  .5; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  .5; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 9.6  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Upper tall sphere for cross
  groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .5   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  .15; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  .05; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 10.2  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  // Upper wide sphere for cross
  groupID[num_objects] = num_pieces;
	obtype[num_objects] = 1;
	color[num_objects][0] =  .6;
  color[num_objects][1] = .3 ; 
  color[num_objects][2] = 0 ;
  reflectivity[num_objects] = 0;
  obtexture[num_objects] = isWhite;
  
	Tn = 0 ;
	Ttypelist[Tn] = RZ ; Tvlist[Tn] =  180   ; Tn++ ;
	Ttypelist[Tn] = SY ; Tvlist[Tn] =  .15   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  .5; Tn++ ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  .05; Tn++ ;
	Ttypelist[Tn] = TX ; Tvlist[Tn] =  x   ; Tn++ ;
  Ttypelist[Tn] = TY ; Tvlist[Tn] =  y + 10.2  ; Tn++ ;
  Ttypelist[Tn] = TZ ; Tvlist[Tn] =  z   ; Tn++ ;
	
	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[num_objects], V, m) ;
  M3d_mat_mult(obinv[num_objects], mi, Vi) ;
  num_objects++;
  
  obj_range[num_pieces][1] = num_objects;
  
}
// End create_king


///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////




// returns -1 for no piece in that position, 0 if piece is black. and 1 if piece is white
int checkForPieces(double position[3]) {
	int i;
	for (i = 0; i < num_pieces; i++) {
		if (position[0] == piece_location[i][0] && position[2] == piece_location[i][2]) {
			return isPieceWhite[i];
		}
	}
	return -1;
}




int movePiece(int piece, double position[3]) {
	int total_objects, total_pieces;
	total_objects = num_objects;
	total_pieces = num_pieces;
	num_pieces = piece;
	num_objects = obj_range[piece][0];
	
	if (pieceID[piece] == 1) {create_pawn(position[0], position[1], position[2], isPieceWhite[piece]);}
	if (pieceID[piece] == 2) {create_rook(position[0], position[1], position[2], isPieceWhite[piece]);}
	if (pieceID[piece] == 3) {create_knight(position[0], position[1], position[2], isPieceWhite[piece]);}
	if (pieceID[piece] == 4) {create_bishop(position[0], position[1], position[2], isPieceWhite[piece]);}
	if (pieceID[piece] == 5) {create_queen(position[0], position[1], position[2], isPieceWhite[piece]);}
	if (pieceID[piece] == 6) {create_king(position[0], position[1], position[2], isPieceWhite[piece]);}
	
	num_objects = total_objects;
	num_pieces = total_pieces;
	
	hasMoved[piece]++;
	return 1;
}



// returns 0 if invalid, 1 if valid, 2 if capture
int checkPath(double start[3], double end[3], int isWhite) {
	double temp[3], diff[2];
	int squares, i, r;
	diff[0] = end[0] - start[0];
	diff[1] = end[2] - start[2];
	
	if (abs(diff[0]) == abs(diff[1])) {squares = abs(diff[0] / 4);} // Diagonal
	else if (abs(diff[0]) > abs(diff[1])) {squares = abs(diff[0] / 4);} // Horizontal
	else {squares = abs(diff[1] / 4);} // Vertical
	
	temp[0] = start[0]; 
	temp[1] = start[1];
	temp[2] = start[2];
	for (i = 0; i < squares - 1; i++) {
		temp[0] = temp[0] + (diff[0] / squares);
		temp[2] = temp[2] + (diff[1] / squares);
		if (checkForPieces(temp) != -1) {
			return 0;
		}
	}
	// Check last square for piece
	temp[0] = temp[0] + (diff[0] / squares);
	temp[2] = temp[2] + (diff[1] / squares);
	r = checkForPieces(temp);
	if (r == !isWhite) {return 2;}
	else if (r == -1) {return 1;}
	
	return 0;
		
}



//returns 0 if not valid, 1 if valid, 2 if capture
int checkPawnMoves(int piece, double position[3]) {
	double positionDiff[2];
	positionDiff[0] = position[0] - piece_location[piece][0]; 
	positionDiff[1] = position[2] - piece_location[piece][2];
	// Account for difference in perspective between colors
	if (!isPieceWhite[piece]) {positionDiff[0] *= -1; positionDiff[1] *= -1;}
	// Check if move is left diagonal
	if (positionDiff[0] == -4 && positionDiff[1] == 4) {
		if (checkForPieces(position) == !isPieceWhite[piece]) {return 2;} 
		else {return 0;}
	}
	//Check if move is right diagonal
	if (positionDiff[0] == 4 && positionDiff[1] == 4) {
		if (checkForPieces(position) == !isPieceWhite[piece]) {return 2;} 
		else {return 0;}
	}
	// Check if move is forward one space
	if (positionDiff[0] == 0 && positionDiff[1] == 4) {
		if (checkForPieces(position) != -1) {return 0;}
		else {return 1;}
	}
	// Check if move is forward two spaces
	if (positionDiff[0] == 0 && positionDiff[1] == 8 && hasMoved[piece] == 0) {
		if (checkPath(piece_location[piece], position, isPieceWhite[piece]) == 1) {return 1;}
	}
	
	return 0;
	
}



//returns 0 if not valid, 1 if valid, 2 if capture
int checkRookMoves(int piece, double position[3]) {
	double positionDiff[2];
	positionDiff[0] = position[0] - piece_location[piece][0]; 
	positionDiff[1] = position[2] - piece_location[piece][2];
	
	// Check that rook moves in a straight line
	if (positionDiff[0] == 0 || positionDiff[1] == 0) {
		return checkPath(piece_location[piece], position, isPieceWhite[piece]);
	}
	
	return 0;
}



// returns 0 if not valid, 1 if valid, 2 if capture
int checkKnightMoves(int piece, double position[3]) {
	double positionDiff[2];
	int check;
	positionDiff[0] = position[0] - piece_location[piece][0]; 
	positionDiff[1] = position[2] - piece_location[piece][2];
	
	// Check that knight moves in L shape
	if ((abs(positionDiff[0]) == 8 && abs(positionDiff[1]) == 4) || (abs(positionDiff[0]) == 4 && abs(positionDiff[1]) == 8)) {
		check = checkForPieces(position);
		if (check == -1) {return 1;}
		else if (check == !isPieceWhite[piece]) {return 2;}
		else {return 0;}
	}
	
	return 0;
}



// returns 0 if not valid, 1 if valid, 2 if capture
int checkBishopMoves(int piece, double position[3]) {
	double positionDiff[2];
	positionDiff[0] = position[0] - piece_location[piece][0]; 
	positionDiff[1] = position[2] - piece_location[piece][2];

	// Check that bishop moves diagonally
	if (abs(positionDiff[0]) == abs(positionDiff[1])) {
		return checkPath(piece_location[piece], position, isPieceWhite[piece]);
	}
	
	return 0;
}



// returns 0 if not valid, 1 if valid, 2 if capture
int checkQueenMoves(int piece, double position[3]) {
	double positionDiff[2];
	positionDiff[0] = position[0] - piece_location[piece][0]; 
	positionDiff[1] = position[2] - piece_location[piece][2];
	
	// Check diagonal path
	if (abs(positionDiff[0]) == abs(positionDiff[1])) {
		return checkPath(piece_location[piece], position, isPieceWhite[piece]);
	}
	// Check horizontal path
	if (positionDiff[0] == 0 || positionDiff[1] == 0) {
		return checkPath(piece_location[piece], position, isPieceWhite[piece]);
	}
	
	return 0;
	
}



// returns 0 if not valid, 1 if valid, 2 if capture, 3 if Queenside Castle, 4 if Kingside Castle
int checkKingMoves(int piece, double position[3]) {
	double positionDiff[2];
	positionDiff[0] = position[0] - piece_location[piece][0]; 
	positionDiff[1] = position[2] - piece_location[piece][2];
	if (hasMoved[piece] == 0 && (abs(positionDiff[0]) == 8 && abs(positionDiff[1]) == 0)) {
		if (isPieceWhite[piece]) {
			if (positionDiff[0] == -8 && hasMoved[8] == 0 && checkPath(piece_location[piece], position, isPieceWhite[piece])) {return 3;}
			if (positionDiff[0] == 8 && hasMoved[15] == 0 && checkPath(piece_location[piece], position, isPieceWhite[piece])) {return 4;}
		} else {
			if (positionDiff[0] == -8 && hasMoved[24] == 0 && checkPath(piece_location[piece], position, isPieceWhite[piece])) {return 3;}
			if (positionDiff[0] == 8 && hasMoved[31] == 0 && checkPath(piece_location[piece], position, isPieceWhite[piece])) {return 4;}
		}
	} else if (abs(positionDiff[0]) <= 4 && abs(positionDiff[1]) <= 4) {
		return checkPath(piece_location[piece], position, isPieceWhite[piece]);
	}		
	return 0;
}



// returns 0 if invalid, 1 if valid, 2 if capture, 3 if Queenside Castle, 4 if Kingside Castle
int moveIsValid(int piece, double position[3]) {
	
	if (hasMoved[piece] == -1) {return 0;}
  else if (pieceID[piece] == 1) {
		return checkPawnMoves(piece, position);
	} else if (pieceID[piece] == 2) {
		return checkRookMoves(piece, position);
	} else if(pieceID[piece] == 3) {
		return checkKnightMoves(piece, position);
	} else if (pieceID[piece] == 4) {
		return checkBishopMoves(piece, position);
	} else if (pieceID[piece] == 5) {
		return checkQueenMoves(piece, position);
	} else if (pieceID[piece] == 6) {
		return checkKingMoves(piece, position);
	} else {return 0;}
}



//return 0 if not in check, 1 if in check
int squareInCheck(double position[3], int isWhite) {
	int i, v;
	if (!isWhite) {
		for (i = 0; i < 16; i++) {
			if (moveIsValid(i, position) != 0) {
				return 1;
			}
		}
	} else {
		for (i = 16; i < 32; i++) {
			if (moveIsValid(i, position) != 0) {
				return 1;
			}
		}
	}
	return 0;
}



//return 0 if not in check, 1 if in check
int willKingBeInCheck(int pieceMoved, double position[3], int kingIsWhite) {
	double oldPosition[3];
	int result, kingID, temp, pieceTemp, i, t;
	if (kingIsWhite) {kingID = 12;}
	else {kingID = 28;}
	
	oldPosition[0] = piece_location[pieceMoved][0];
	oldPosition[1] = piece_location[pieceMoved][1];
	oldPosition[2] = piece_location[pieceMoved][2];
	t = 0;
	//if (moveIsValid(pieceMoved, position) == 0) {return 0;}
	for (i = 0; i < num_pieces; i++) {
		if (position[0] == piece_location[i][0] && position[2] == piece_location[i][2]) {
			temp = hasMoved[i]; 
			pieceTemp = i;
			hasMoved[i] = -1;
			t = 1;
		}
	}
	movePiece(pieceMoved, position);
	result = squareInCheck(piece_location[kingID], kingIsWhite);
	movePiece(pieceMoved, oldPosition);
	hasMoved[pieceMoved] -= 2;
	if (t) {hasMoved[pieceTemp] = temp;}

	return result;
}



//return 0 if not checkmate, 1 if checkmate
int isCheckmate(int isWhite) {
	int i, x, z, istart, istop, count;
	double position[3];
	if (!isWhite) {istart = 16; istop = 32;}
	else {istart = 0; istop = 16;}
	count = 0;
	for (x = -4; x < 4; x++) {
		for (z = -4; z < 4; z++) {
			position[0] = x * 4 + 2; position[1] = 0; position[2] = z * 4 + 2;
			for (i = istart; i < istop; i++) {
				if (moveIsValid(i, position) && !willKingBeInCheck(i, position, isWhite)) {return 0;}
			}
		}
	}
	return 1;
}




// for side: 0 = queenside, 1 = kingside
// returns 0 if invalid, 1 if valid 
int castle(int piece, int side) {
	int queenRookID, kingRookID;
	double rookPosition[3], kingPosition[3], checkPosition[3];
	if (isPieceWhite[piece]) {
		queenRookID = 8;
		kingRookID = 15;
	} else {
		kingRookID = 31;
		queenRookID = 24;
	}
	if (side == 0) {
		checkPosition[0] = piece_location[piece][0] - 4;
		checkPosition[1] = piece_location[piece][1];
		checkPosition[2] = piece_location[piece][2];
		if (squareInCheck(checkPosition, isPieceWhite[piece])) {return 0;}
		rookPosition[0] = piece_location[queenRookID][0] + 12;
		rookPosition[1] = piece_location[queenRookID][1];
		rookPosition[2] = piece_location[queenRookID][2];
		kingPosition[0] = piece_location[piece][0] - 8;
		kingPosition[1] = piece_location[piece][1];
		kingPosition[2] = piece_location[piece][2];
		movePiece(queenRookID, rookPosition);
		movePiece(piece, kingPosition);
	} else if (side == 1) {
		checkPosition[0] = piece_location[piece][0] + 4;
		checkPosition[1] = piece_location[piece][1];
		checkPosition[2] = piece_location[piece][2];
		if (squareInCheck(checkPosition, isPieceWhite[piece])) {return 0;}
		rookPosition[0] = piece_location[kingRookID][0] - 8;
		rookPosition[1] = piece_location[kingRookID][1];
		rookPosition[2] = piece_location[kingRookID][2];
		kingPosition[0] = piece_location[piece][0] + 8;
		kingPosition[1] = piece_location[piece][1];
		kingPosition[2] = piece_location[piece][2];
		movePiece(kingRookID, rookPosition);
		movePiece(piece, kingPosition);
	}
	
	return 1;
}




int capture(double position[3]) {
	int i;
	
	for (i = 0; i < num_pieces; i++) {
		if (position[0] == piece_location[i][0] && position[2] == piece_location[i][2]) {
			hasMoved[i] = -1;
			if (isPieceWhite[i]) {
				movePiece(i, captureLocation[1]);
				if (captureLocation[1][2] <= -2) {
					captureLocation[1][0] -= 4;
					captureLocation[1][2] = 18;
				} else {captureLocation[1][2] -= 4;}
			} else {
				movePiece(i, captureLocation[0]);
				if (captureLocation[0][2] >= 2) {
					captureLocation[0][0] += 4;
					captureLocation[0][2] = -18;
				} else {captureLocation[0][2] += 4;}
			}
		}
	}	
}





int waitMove() {
	int complete, obj_hit, piece_hit, validity;
	double p[2], board_pt[3], new_position[3], rgb[3], rayEnd[3];
	
	complete = 0;
	
	while (!complete) {
		G_wait_click(p);
		rayEnd[0] = (2 * p[0] * Tan_half_angle) / window_width - Tan_half_angle;
		rayEnd[1] = (2 * p[1] * Tan_half_angle) / window_height - Tan_half_angle;
		rayEnd[2] = 1;
		obj_hit = ray(rgb, EYE, rayEnd, board_pt);
		if (obj_hit <= 0) {continue;}
		piece_hit = groupID[obj_hit];
		G_wait_click(p);
		rayEnd[0] = (2 * p[0] * Tan_half_angle) / window_width - Tan_half_angle;
		rayEnd[1] = (2 * p[1] * Tan_half_angle) / window_height - Tan_half_angle;
		rayEnd[2] = 1;
		board_pt[1] = -1;
		ray(rgb, EYE, rayEnd, board_pt);
		if (board_pt[1] != 0) {continue;}
		new_position[0] = floor(board_pt[0] * 5) * 4 + 2;
		new_position[1] = 0;
		new_position[2] = floor(board_pt[2] * 5) * 4 + 2;
		validity = moveIsValid(piece_hit, new_position);
		if (isPieceWhite[piece_hit] == !isWhiteTurn) {validity = 0;}
		if (validity == 0) {continue;}
		if (isWhiteTurn && willKingBeInCheck(piece_hit, new_position, isWhiteTurn)) {continue;}
		if (!isWhiteTurn && willKingBeInCheck(piece_hit, new_position, isWhiteTurn)) {continue;}
		if (validity == 1) {movePiece(piece_hit, new_position);}
		if (validity == 2) {
		capture(new_position); 
		movePiece(piece_hit, new_position);
		}
		if (validity == 3) {if (!castle(piece_hit, 0)) {continue;}}
		if (validity == 4) {if (!castle(piece_hit, 1)) {continue;}}
		
		if (squareInCheck(piece_location[12], 1)) {isInCheck[1] = 1;}
		else {isInCheck[1] = 0;}
		if (squareInCheck(piece_location[28], 0)) {isInCheck[0] = 1;}
		else {isInCheck[0] = 0;}
		
		
		if (isInCheck[!isWhiteTurn]) {
			if (isCheckmate(!isWhiteTurn)) {
				gameOver = 1;
				whiteWon = isWhiteTurn;
			}
		}
		
		complete = 1;
	}
	
}



void create_board() {
	int Ttypelist[10], Tn, key;
  double Tvlist[10], m[4][4], mi[4][4];
	obtype[0] = 0;
	color[0][0] = 1;
	color[0][1] = 1;
	color[0][2] = 1;
	reflectivity[0] = 0;
	obtexture[0] = 2;
	
	Tn = 0 ;
	Ttypelist[Tn] = SZ ; Tvlist[Tn] =  20   ; Tn++ ;
	Ttypelist[Tn] = SX ; Tvlist[Tn] =  20   ; Tn++ ;

	M3d_make_movement_sequence_matrix(m, mi, Tn, Ttypelist, Tvlist);
  M3d_mat_mult(obmat[0], V, m) ;
  M3d_mat_mult(obinv[0], mi, Vi) ;
}



void update_board() {
	int i;
	create_board();
	for (i = 0; i < num_pieces; i++) {
		movePiece(i, piece_location[i]);
		hasMoved[i]--;
	}
}





void fill_board() {
	//White Pieces
	create_pawn(-14, 0, -10, 1);
	num_pieces++;
	create_pawn(-10, 0, -10, 1);
	num_pieces++;
	create_pawn(-6, 0, -10, 1);
	num_pieces++;
	create_pawn(-2, 0, -10, 1);
	num_pieces++;
	create_pawn(2, 0, -10, 1);
	num_pieces++;
	create_pawn(6, 0, -10, 1);
	num_pieces++;
	create_pawn(10, 0, -10, 1);
	num_pieces++;
	create_pawn(14, 0, -10, 1);
	num_pieces++;
	create_rook(-14, 0, -14, 1);
	num_pieces++;
	create_knight(-10, 0, -14, 1);
	num_pieces++;
	create_bishop(-6, 0, -14, 1);
	num_pieces++;
	create_queen(-2, 0, -14, 1);
	num_pieces++;
	create_king(2, 0, -14, 1);
	num_pieces++;
	create_bishop(6, 0, -14, 1);
	num_pieces++;
	create_knight(10, 0, -14, 1);
	num_pieces++;
	create_rook(14, 0, -14, 1);
	num_pieces++;
	
	//Black Pieces
	create_pawn(-14, 0, 10, 0);
	num_pieces++;
	create_pawn(-10, 0, 10, 0);
	num_pieces++;
	create_pawn(-6, 0, 10, 0);
	num_pieces++;
	create_pawn(-2, 0, 10, 0);
	num_pieces++;
	create_pawn(2, 0, 10, 0);
	num_pieces++;
	create_pawn(6, 0, 10, 0);
	num_pieces++;
	create_pawn(10, 0, 10, 0);
	num_pieces++;
	create_pawn(14, 0, 10, 0);
	num_pieces++;
	create_rook(-14, 0, 14, 0);
	num_pieces++;
	create_knight(-10, 0, 14, 0);
	num_pieces++;
	create_bishop(-6, 0, 14, 0);
	num_pieces++;
	create_queen(-2, 0, 14, 0);
	num_pieces++;
	create_king(2, 0, 14, 0);
	num_pieces++;
	create_bishop(6, 0, 14, 0);
	num_pieces++;
	create_knight(10, 0, 14, 0);
	num_pieces++;
	create_rook(14, 0, 14, 0);
	num_pieces++;
	
	int i;
	
	for (i = 0; i < num_pieces; i++) {
		hasMoved[i] = 0;
	}
}




///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

int main ()
{
  FILE *f ;
  double light_in_worldspace[3];
  int Ttypelist[10], Tn, key;
  double Tvlist[10], m[4][4], mi[4][4];
  char fname[50];


  window_width = 800 ; window_height = 800 ;
  // size of largest square Inside window
  if (window_width < window_height) { window_square_size = window_width ; }
                               else { window_square_size = window_height ; }
  Half_window_size = 0.5*window_square_size ;
  Half_angle_degrees = 30 ;
  Tan_half_angle = tan(Half_angle_degrees*M_PI/180) ;
  if (window_width > MAX_WINDOW_WIDTH) {
  	printf("Window width too large");
  	exit(0);
  }
  else if (window_height > MAX_WINDOW_HEIGHT) {
  	printf("Window height too large");
  	exit(0);
  }
	
  //init_z_buffer();
  G_init_graphics(window_width, window_height);
  
  
  mapID[0] = init_xwd_map_from_file("GraniteJ.xwd");
  if (mapID[0] == -1) {printf("Texture map file does not exist\n"); exit(0);}
  mapID[1] = init_xwd_map_from_file("WhiteMarbleJ.xwd");
  if (mapID[1] == -1) {printf("Texture map file does not exist\n"); exit(0);}
  mapID[2] = init_xwd_map_from_file("chess_boardJ.xwd");
  if (mapID[2] == -1) {printf("Texture map file does not exist\n"); exit(0);}
  
  light_in_worldspace[0] =  150 ;
  light_in_worldspace[1] =  150 ;
  light_in_worldspace[2] = 50 ;
  EYE[0] = 0;
  EYE[1] = 0;
  EYE[2] = 0;
  
  double eye[3], coi[3], up[3];
  
  eye[0] = 0; eye[1] = 35; eye[2] = -30;
  coi[0] = 0; coi[1] = 0; coi[2] = 0;
  up[0] = eye[0]; up[1] = eye[1] + 1 ; up[2] = eye[2];
  
  M3d_view(V, Vi, eye, coi, up);
	M3d_mat_mult_pt(light_in_eye_space, V, light_in_worldspace);
	
	num_objects = 0;
	num_pieces = 0;
	captureLocation[0][0] = 22; captureLocation[0][1] = 0; captureLocation[0][2] = -18;
	captureLocation[1][0] = -22; captureLocation[1][1] = 0; captureLocation[1][2] = 18;
	isInCheck[0] = 0;
	isInCheck[1] = 0;
	
	create_board();
	num_objects++;
	
	fill_board();
	
	draw();
	isWhiteTurn = 1;
	while (!gameOver) {
		waitMove();	
		isWhiteTurn = !isWhiteTurn;
		if (isWhiteTurn) {eye[0] = 0; eye[1] = 35; eye[2] = -30; up[0] = eye[0]; up[1] = eye[1] + 1 ; up[2] = eye[2];} 
		else {eye[0] = 0; eye[1] = 35; eye[2] = 30; up[0] = eye[0]; up[1] = eye[1] + 1 ; up[2] = eye[2];}
		M3d_view(V, Vi, eye, coi, up);
		M3d_mat_mult_pt(light_in_eye_space, V, light_in_worldspace);
		update_board();
		draw();
		
 } 
 
 if (whiteWon) {
 	printf("White Wins!\n");
 }else {
 	printf("Black Wins!\n");
 }
 
 G_wait_key();
 
}























