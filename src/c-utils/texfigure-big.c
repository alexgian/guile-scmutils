/* -*-C-*-

$Id: copyright.c,v 1.4 2005/12/13 06:40:58 cph Exp $

Copyright 2005 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/
#include <stdio.h>
#include <math.h>

#define MAXLABELS 20

#define TEXWIDTH 1664
#define TEXHEIGHT 1664
/*
#define TEXWIDTH 1024
#define TEXHEIGHT 1024
*/

main(argc, argv)
     int argc;
     char **argv;
{
  int xintervals=4, yintervals=4, nxv=0, nyv=0;
  int i=1;
  char *xvalues[MAXLABELS], *yvalues[MAXLABELS], *xlabel, *ylabel, *name;
  char name_tex[128];
  FILE *out;
  int setxv=0, setyv=0;
  int xlabeldef=0, ylabeldef=0;

  if(argc == 1) {
    printf("%s -n name -xi xintervals -yi yintervals -xl xlabel -yl ylabel -xv xvalues -yv yvalues\n", argv[0]);
    exit(-1);
  }
  
  while(i < argc) {
    if(!strncmp(argv[i], "-n", 2)) {i++;  setxv=0; setyv=0; name = argv[i]; continue;}
    if(!strncmp(argv[i], "-xi", 3)) {i++; setxv=0; setyv=0; xintervals = atoi(argv[i]); continue;}
    if(!strncmp(argv[i], "-yi", 3)) {i++; setxv=0; setyv=0; yintervals = atoi(argv[i]); continue;}
    if(!strncmp(argv[i], "-xl", 3)) {i++; setxv=0; setyv=0; xlabel = argv[i]; xlabeldef=1; continue;}
    if(!strncmp(argv[i], "-yl", 3)) {i++; setxv=0; setyv=0; ylabel = argv[i]; ylabeldef=1; continue;}
    if(!strncmp(argv[i], "-xv", 3)) {i++; setxv=1; setyv=0; continue;}
    if(!strncmp(argv[i], "-yv", 3)) {i++; setyv=1; setxv=0; continue;}

    if(setyv) {yvalues[nyv++] = argv[i++]; continue;}
    if(setxv) {xvalues[nxv++] = argv[i++]; continue;}

    i++;
  }

  strcpy(name_tex, name);
  strcat(name_tex, ".tex"); 
  out = fopen(name_tex, "w");
  
  {
    double x, y, dx, dy;
    double border = 30;
    double label_edge = -5.0;
    double x_size,  y_size, x_size_1, y_size_1, x_total, y_total;
    double x_interval, y_interval, t_size=5.0;
    int x_tics, y_tics;
    char x_size_str[128], y_size_str[128], x_bit_str[128], y_bit_str[128];
    char temp[128];

    x_size = TEXWIDTH  * 0.2409; x_size_1 = x_size + 1.0;
    y_size = TEXHEIGHT * 0.2409; y_size_1 = y_size + 1.0;

    x_total = x_size_1 + border + border;
    y_total = y_size_1 + border + border;

    x_interval = x_size_1 / ((double) xintervals);
    y_interval = y_size_1 / ((double) yintervals);

    x_tics = xintervals - 1;
    y_tics = yintervals - 1;

    sprintf(x_size_str, "%lf", x_size);
    sprintf(y_size_str, "%lf", y_size);

    sprintf(x_bit_str, "%lf", TEXWIDTH);
    sprintf(y_bit_str, "%lf", TEXHEIGHT);

    fprintf(out, "\\setlength{\\unitlength}{1pt}\n");
    fprintf(out, "\\begin{picture}( %lf , %lf )( %lf , %lf )\n", x_total, y_total, -border, -border);

    if(nxv>2) {
      dx = x_size_1 / ((double) (nxv - 1));
      x = 0.0;
      fprintf(out, "  \\putmbox { %lf , %lf }{ %s }{ $%s$ }\n", x, label_edge, "tl", xvalues[0]);
      for(i=1; i<nxv-1; i++) {
	x += dx;
	fprintf(out, "  \\putmbox { %lf , %lf }{ %s }{ $%s$ }\n", x, label_edge, "t", xvalues[i]);
      }
      fprintf(out, "  \\putmbox { %lf , %lf }{ %s }{ $%s$ }\n", x_size_1, label_edge, "tr", xvalues[nxv-1]);
    }

    if(nyv>2) {
      dy = y_size_1 / ((double) (nyv - 1));
      y = 0.0;
      fprintf(out, "  \\putmbox { %lf , %lf }{ %s }{ $%s$ }\n", label_edge, y, "br", yvalues[0]);
      for(i=1; i<nyv-1; i++) {
	y += dy;
	fprintf(out, "  \\putmbox { %lf , %lf }{ %s }{ $%s$ }\n", label_edge, y, "r", yvalues[i]);
      }
      fprintf(out, "  \\putmbox { %lf , %lf }{ %s }{ $%s$ }\n", label_edge, y_size_1, "tr", yvalues[nyv-1]);
    }


    if(xlabeldef == 1)
      fprintf(out, "  \\putmbox { %lf , %lf }{ %s }{ $%s$ }\n", 0.5*x_size_1, -0.5*border, "t", xlabel);

    if(ylabeldef == 1)
      fprintf(out, "  \\putmbox { %lf , %lf }{ %s }{ $%s$ }\n", -border, 0.5*y_size_1, "r", ylabel);

    fprintf(out, "  \\special{psfile=%s.ps}\n", name);


    fprintf(out, "  \\put(0,0){\\framebox ( %lf , %lf ) {}}\n", x_size_1, y_size_1);

    fprintf(out, "  \\multiput(%lf,%lf)(%lf,%lf){%d}{\\line%s{%lf}}\n", 
	    x_interval, 0.0, x_interval, 0.0, x_tics, "(0,1)", t_size);
    fprintf(out, "  \\multiput(%lf,%lf)(%lf,%lf){%d}{\\line%s{%lf}}\n", 
	    x_interval, y_size_1, x_interval, 0.0, x_tics, "(0,-1)", t_size);
    fprintf(out, "  \\multiput(%lf,%lf)(%lf,%lf){%d}{\\line%s{%lf}}\n", 
	    0.0, y_interval, 0.0, y_interval, y_tics, "(1,0)", t_size);
    fprintf(out, "  \\multiput(%lf,%lf)(%lf,%lf){%d}{\\line%s{%lf}}\n", 
	    x_size_1, y_interval, 0.0, y_interval, y_tics, "(-1,0)", t_size);

    fprintf(out, "\\end{picture}\n");

  }

}
