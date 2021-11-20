USERID KICKS
/*  kkpgm.c */

    #include <stdio.h>
    #include <stdlib.h>
    #include <limits.h>
    #include <string.h>
    #include <ctype.h>
    #include <math.h>
    #include <time.h>

#include "dfhaid.h"
#include "kkmsd.h"		/* symbolic map */

      void Scr_out(void);
      void Clear_pr(void);
      void hdr_msg(void);
    double Expression(void);
       int IsAddop(char);
    double Term(void);
    double Factor(void);
      void Match(char);
       int IsAddop(char);
       int IsMultop(char);
       int Is_White(char);
      void SkipWhite(void);
    double GetNum(void);
      void Save_wsp(void);
      void Load_wsp(void);
      void Get_date(void);
      void Get_time(void);
      void View_dsp(void);
      void Sum_proc(void);
      void Avg_proc(void);
      void Rng_proc(void);
      void Sqrt_pr(void);
      void Date_pr(void);
      void Mth_proc(void);
      void Day_proc(void);
      void Yr_proc(void);
      void Time_pr(void);
      void Hour_pr(void);
      void Min_proc(void);
      void Sec_proc(void);
      void Print_pr(void);
    double asc_2_dbl(void);
      void Pow_dsp(void);
   
    double sum = 0;
    double avg = 0;
    double value;

    int maxm = INT_MIN;
    int minm = INT_MAX;
    int proc_cel = 0;
    int cnt = 0;
    int epos = 0;
    int mrow = 0;
    int mcol = 0;
    int rct = 0;		/* current rpow setting */
    int cct = 1;		/* current col setting */
    int row;
    int col;
    int x;
    int z1;
    int z2;
    int z3;
    int z4;
    int i;
    int j;
    int pi;
    int day, month, year;
    int hour, min, sec;
    int t,i1,j1,i2,j2,k,l,m,n,row,col;
    int jj,i9;
    int sv_rct;
    int sv_cct;
    int exmode;
    int bots = 16;

    char input[20];
    char cellnum[6];
    char varname[4];
    char wkspace[9];
    char wk_row[4];
    char wk_row1[4];
    char ch;
    char wk_file[12];
    char *p;
    char *p1;
    char *p2;
    char sheet_nm[9];
    char wsp_name[9];
    char f_date[11];
    char f_mth[3];
    char f_day[3];
    char f_yr[5];
    char f_time[9];
    char f_hr[3];
    char f_min[3];
    char f_sec[3];

    char alpha[26] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

#define wsize                    30

    double spreadsheet[wsize][27] = {0};
      char sheet[wsize][27][20]   = {0};
      char src[wsize][27][20]     = {0};
       int flag[wsize][27]        = {0};

      FILE *fp;

    int main(KIKEIB *eib)
    {
       int opa, ops, opm, opd;
       int u2;
       int u3;
       int u4;
           
       char fblk3[3];
       char fblk18[18];

       Clear_pr();
       rct = 0;
       cct = 1;
       exmode = 1;
       strcpy(mapa.mapao.wsheeto, "CELL");
       strcpy(wkspace, "EMPTY"); 

       Scr_out();

       while(1)
       {

start1:
          
         EXEC CICS SEND
              MAP("mapa") MAPSET("kkmsd") ERASE
              ;

         EXEC CICS RECEIVE
              MAP("mapa") MAPSET("kkmsd") NOHANDLE
              ;

         if(eib->eibaid == KIKPF1)		/* PF1  EXIT */
         {
             break;
         }

         if(eib->eibaid == KIKPF2)		/* PF2  Load */
         {
             exmode = 1;
             Load_wsp();
             goto skip_save;
         } 
 
         if(eib->eibaid == KIKPF3)		/* PF3  Save */
         {
             exmode = 1;
             Save_wsp();
             goto print_loop;
         }

         if(eib->eibaid == KIKPF4)		/* PF4  New  */
         {
             Clear_pr();
             rct = 0;
             cct = 1;
             exmode = 1;
             strcpy(mapa.mapao.wsheeto, "CELL");
             strcpy(wkspace, "EMPTY"); 
             goto print_loop;
         }

         if(eib->eibaid == KIKPF5)		/* PF5  Cell */
         {
             exmode = 1;
             goto skip_save;
         }

         if(eib->eibaid == KIKPF6)		/* PF6  Prog */
         {
             exmode = 2;
             goto print_loop;
         } 

         if(eib->eibaid == KIKPF7)		/* PF7  Home */
         {
             sv_rct = 1;
             sv_cct = 1;
             exmode = 1;
             goto skip_save;
         } 

         if(eib->eibaid == KIKPF8)		/* PF8  BOT  */
         {
             sv_rct = bots;
             exmode = 1;
             goto print_loop;
         } 

         if(eib->eibaid == KIKPF9)		/* PF9  TOP  */
         {
             sv_rct = 1;
             exmode = 1;
             goto skip_save;
         } 

         if(eib->eibaid == KIKPF10)	/* PF10  LEFT  */
         {
             sv_cct = sv_cct - 4;
             if(sv_cct < 1)
             {
                sv_cct = 1;
             }
             exmode = 1;
             goto skip_save;
         } 

         if(eib->eibaid == KIKPF11)	/* PF11  RIGHT  */
         {
             sv_cct = sv_cct + 4;
             exmode = 1;
             goto skip_save;
         } 

         if(eib->eibaid == KIKPF12)	/* PF12  VIEW  */
         {
             view_dsp();
             exmode = 1;
             goto skip_save;
         } 

         strcpy(cellnum, mapa.mapai.celli);
         strcpy(input, mapa.mapai.valuei);

         if(cellnum[0] == ' ')
         {
            goto skip_save;
         }

         exmode = 1;
         u2 = 0;
         u4 = 18;
         for(u3 = 0; u3 <= 18; u3++)
         {
            u2 = isalnum(input[u4]); 
            if(u2 == 0)
            {
               if(input[u4] == ')')
               {
                 input[u4+2] = '\0';
                 break;
               }
            }
            else
            {
               input[u4+2] = '\0';
               break;
            }
            u4--;
          }
        
          if(cellnum[0] == 'Z')
          {
             col = 26;
             wk_row[0] = cellnum[1];
             wk_row[1] = cellnum[2];
             wk_row[2] = '\0';
             row = atoi(wk_row);     
          }
          else
          {
            if((cellnum[0] >= 'A') && (cellnum[0] <= 'Z'))
               col = cellnum[0] - 'A'+1;
            else
               z2 = 0;
               for(z1 = 1; z1 <= 26; z1++)
               {
                  if(cellnum[0] == sheet[z2][z1][0])
                  {
                     col = z1;
                     break;
                  }
               }

            wk_row[0] = cellnum[1];
            wk_row[1] = cellnum[2];
            wk_row[2] = '\0';
            row = atoi(wk_row);        /* now have row number */
         }

	   opa = 0;
          ops = 0;
          opm = 0;
          opd = 0;

          x = strlen(input);
          input[x-1] = '\0';

          /*  load source (program) */
          strcpy(src[row][col], input);
                     
          p = strstr(input, "=");

          if(!p) 
          {
               p2 = strstr(input, "-");
               if(p2)
               {
                  p1 = strstr(input, ".");
                  if(p1)
                  {
                     value = atof(input);
                     spreadsheet[row][col] = value;
                     flag[row][col] = 1;
                     sprintf(sheet[row][col], "%.2f", value);
                  }
                  else
                  {
                     z4 = atoi(input);
                     sprintf(sheet[row][col], "%d", z4);
                     flag[row][col] = 1; 
                     spreadsheet[row][col] = z4;
                  }
               }
          }

          if(!p) 
          { 
             if(isNumber(input))        /*  numeric cell */
             {
                p1 = strstr(input, ".");
                if(p1)
                {
                   spreadsheet[row][col] = atof(input);
                }
                else
                {
                   spreadsheet[row][col] = atoi(input);
                }
                flag[row][col] = 1;
                strcpy(sheet[row][col],input);
                if(strlen(sheet[row][col]) > 18)
                {
                   sheet[row][col][18] = '\0';
                }
             }
             else                    /* text cell */     
             {
                strcpy(sheet[row][col], input);
                sheet[row][col][18] = '\0';
                if(strlen(sheet[row][col]) > 18)
                {
                   sheet[row][col][18] = '\0';
                }
             }
        }

skip_save:

        /* *** Re-Compute *** */
       
        for(i9 = 1; i9 <= wsize; i9++)
        {
           for(jj = 1; jj <= 26; jj++)  		/* i9 = row  jj = col */
           {
              proc_cel = 0;
              strcpy(input, src[i9][jj]);

              p = strstr(input, "_");
              if(!p)
              { 
                 opa = 0;
                 ops = 0;
                 opm = 0;
                 opd = 0;

                 p = strstr(input, "+");
                 if(p)
                    opa = 1;

                 p = strstr(input, "-");
                 if(p)
                    ops = 1;

                 p = strstr(input, "*");
                 if(p) 
                    opm = 1;

                 p = strstr(input, "/");
                 if(p)
                    opd = 1;

                 if((opa == 1) || (ops == 1) || (opm == 1) || (opd == 1))
                 {
                     pi = 0;
                     z3 = 1;
                     z4 = 1;
                     value = Expression();
                     sprintf(sheet[i9][jj],"%.2f",value);
                     proc_cel = 1;
                 }

                 p1 = strstr(input, "=");
                 p2 = strstr(input, "-");

                 if((!p1) && (p2))
                 {
                     p1 = strstr(input, ".");
                     if(p1)
                     {
                        value = spreadsheet[i9][jj];
                        sprintf(sheet[i9][jj], "%.2f", value);
                     }
                     else
                     {
                        z4 = spreadsheet[i9][jj];
                        sprintf(sheet[i9][jj], "%d", z4);
                     }
                 }
   
               p = strstr(input, "SUM");
               if(p)
               {
                  Sum_proc();
               }
               p = strstr(input, "AVG");
               if(p)
               {
                  Avg_proc();
               }
               p = strstr(input, "RNG");
               if(p)
               {
                  Rng_proc();
               } 
               p = strstr(input, "DATE");
               if(p)
               {
                  Date_pr();
               } 
               p = strstr(input, "MTH");
               if(p)
               {
                  Mth_proc();
               } 
               p = strstr(input, "DAY");
               if(p)
               {
                  Day_proc();
               }
               p = strstr(input, "YR");
               if(p)
               {
                  Yr_proc();
               }
               p = strstr(input, "TIME");
               if(p)
               {
                  Time_pr();
               }
               p = strstr(input, "HOUR");
               if(p)
               {
                  Hour_pr();
               }
               p = strstr(input, "MIN");
               if(p)
               {
                  Min_proc();
               }
               p = strstr(input, "SEC");
               if(p)
               {
                  Sec_proc();
               }
               p = strstr(input, "SQRT");
               if(p)
               {
                  Sqrt_pr();
               }
               p = strstr(input, "POW");
               if(p)
               {
                  Pow_dsp();
               }
            }
          }
        }
 
print_loop:
      
        strcpy(fblk3, "  ");
        strcpy(mapa.mapai.valuei, fblk3);  
        strcpy(fblk18, "                 "); 
        strcpy(mapa.mapai.celli, fblk18);
       
        rct = sv_rct;
        cct = sv_cct;

        if(exmode == 1)
        {
           Scr_out();
        }
        if(exmode == 2)
        {
           Scr_out();
        }


     }               /* end of while(1) loop */

     /* If PF1 pressed, here */
         EXEC KICKS XCTL PROGRAM("KSSFPGM") NOHANDLE ;  
}


int isNumber(char str[20])
{
   int i,j,k,flag = 1;
   for(i = 0; str[i] != '\0'; i++)
   {
      if((str[i] >= '0' && str[i] <= '9') || (str[i] == '.'))
      {}
      else
      {
         flag = 0;
         break;
      }
   }
   return flag;
}


double Expression()	
{
   char ch;
   
   double Value;

   z1 = 0;
   z4 = 0;
 
   ch = input[pi];
   if(ch != '=')
   {
      Value = 0;
      return;
   }

   while(ch != '=')
   {
      pi++;
      ch = input[pi];
   }
   pi++; 
   ch = input[pi];		/* got cell for math */
   varname[0] = ch;
   varname[1] = '\0';
   epos = pi;
   if(IsAddop(ch))
   {  
      Value = 0;  		
   }
   else
   {
     Value = Term();
     pi = epos;
     ch = input[pi];
   }

   while(IsAddop(ch))
   {
     switch(ch)
     {
        case '+':
          Match('+');
          Value = Value + Term();
          break;
    
        case '-':
          Match('-');
          Value = Value - Term();
          break;

        default:
          break;
     }
     pi = epos;
     ch = input[pi];
  }
  return Value;
}



double Term()		
{   
   char ch;
   int pi;
   double Value;

   Value = Factor();

   pi = epos;
   ch = input[pi];
   while(IsMultop(ch))
   {
     switch(ch)
     {
        case '*':
          Match('*');
          Value = Value * Factor();   
          break;
    
        case '/':
          Match('/');
          Value = Value / Factor();   
          break;

        case '^':
          Match('^');
          Value = pow(Value, Factor()); 
          break;

        case '%':
          Match('%');
          Value = (int) Value % (int) Factor();     
          break;

        default:
          break;
     }
     pi = epos;
     ch = input[pi];
  }
  return Value;
}

double Factor()                        
{ 
   char ch;
   int pi;
   double value;

   pi = epos; 
   ch = input[pi];

/* printf("\nFACTOR #1 ch = %c pi = %d input = %s\n",ch,pi,input); */

   if(ch == '(')
   {
     Match('(');
     value = Expression();
     Match(')');
   }
   else
   {
     if(isalpha(ch))			
     {
        z2 = 0;
        z1 = 0;
        ch = input[pi];
        x = strlen(input);

        for(z3 = z4; z3 <= x; z3++)
        {
           if(ch == ' ')
           {
              break;
           }
           if(ch == '+')
           {
              break;
           }
           if(ch == '-')
              break;
           if(ch == '*')
              break;
           if(ch == '/')
              break;
           if(ch == ' ')
              break;
           
           if(ch != '=')
           {
              wk_row[z2] = ch;
              z2++;
           }
           z1++;
           pi++;
           ch = input[pi];
        }
        wk_row[z2] = '\0';
/* printf("FACTOR #5 zi = %d 2 = %d z3 = %d ch = %c\n",pi,z2,z3,ch); */

        z3++;
        z3++;
        z4 = z3;

        x = strlen(wk_row);
        if(x == 2)
        {
             if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
             {
                z2 = 0;
                for(z1 = 1; z1 <= 26; z1++)
                {
                   if(wk_row[0] == sheet[z2][z1][0])
                   {
                      mcol = z1;
                      break;
                   }
                }
             }
               
             else
                z2 = 0;
                for(z1 = 1; z1 <= 26; z1++)
                {
                   if(wk_row[0] == sheet[z2][z1][0])
                   {
                      mcol = z1;
                      break;
                   }
                }
              mrow = wk_row[1] - '1'+1;  /* now have row number */
        }

        if(x == 3)
        {
             if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
             {
                z2 = 0;
                for(z1 = 1; z1 <= 26; z1++)
                {
                   if(wk_row[0] == sheet[z2][z1][0])
                   {
                      mcol = z1;
                      break;
                   }
                }
             }
             else
             
                z2 = 0;
                for(z1 = 1; z1 <= 26; z1++)
                {
                   if(wk_row[0] == sheet[z2][z1][0])
                   {
                      mcol = z1;
                      break;
                   }
                }
                wk_row1[0] = wk_row[1];
                wk_row1[1] = wk_row[2];
                wk_row1[2] = '\0';
                mrow = atoi(wk_row1);  
           }  

          value = spreadsheet[mrow][mcol];

          epos = pi;
     }
     else				
     {
         value = GetNum(); 
     }
  }
  return value;
}


double GetNum()			
{   
   char ch;
   int pi;
   double value=0;

   pi = epos;
   ch = input[pi];
   if((!isdigit(ch)) && (ch != '.'))
   {
     /*strcpy(t_holder, "Numeric Value"); */
     
   }
   value = asc_2_dbl();
/*
   pi = e_pos;
   ch = p_string[pi];
   if(isdigit(ch))
   {
     while(isdigit(ch))
     {
       pi++;
       ch = p_string[pi];
     }
     e_pos = pi;
  }
  SkipWhite();
*/
  return value;
}


double asc_2_dbl()
{   
   char ch, cvalue[33];
   int pi, vi_pos=0;
   double fvalue;

   pi = epos;
   ch = input[pi];
   while((isdigit(ch)) || (ch == '.') && (vi_pos <= 32))
   {
     cvalue[vi_pos] = ch;
     pi++;
     vi_pos++;
     ch = input[pi];
   }
   cvalue[vi_pos] = '\0';
   fvalue = atof(cvalue);                 
   epos = pi;
   return fvalue;
}


int IsAddop(char ch) 		
{   
   int rval=0;

   if((ch == '+') || (ch == '-'))
   {
     rval = 1;
   }
   return rval;
}


int IsMultop(char ch) 		
{   
   int rval=0;

   if(ch == '\0')
   {                       
     rval = 0;           
   }
   else if(strchr("*^/%", ch))
   {
     rval = 1;
   }
   return rval;
}

void Match(char x)             
{   
   char ch, string[6];
   int pi;

   pi = epos;
   ch = input[pi];
   if(ch != x)
   {
     strcpy(string, "\" \"");
     string[1] = x;
   }
   else
   {
     epos++;
     SkipWhite();
   }
}

void SkipWhite() 	
{   
   char ch;
   int pi;

   pi = epos;
   ch = input[pi];
   while(Is_White(ch))
   {
     epos++;
     pi = epos;
     ch = input[pi];
   }
}


int Is_White(char ch)
{   
   int test=0;

   if((ch == ' ') || (ch == '\t'))
   {
     test = -1;
   }
   return test;
}

void Save_wsp()
{
   int i,j,x,k9;
   int u2;
   int u3;
   int u4;

   char str1[20];
   char strc[3];
   char strr[3];
   char str[3];
   char tot_nm[5];

   EXEC CICS SEND
        MAP("mapb") MAPSET("kkmsd") ERASE
        ;

   EXEC CICS RECEIVE
        MAP("mapb") MAPSET("kkmsd") NOHANDLE
        ;

   strcpy(wk_file, mapb.mapbi.swkspi);
   strcpy(mapa.mapao.wspnmo, wk_file); 

   for(i = 0; i < 9; i++)
   {
      if(isalnum(wk_file[i]))
      {
      }
      else
      {
         wk_file[i] = '\0';
         break;
      }
   }
   strcat(wk_file, " wps A"); 

   fp = fopen(wk_file, "w");

   k9 = 0;
   for(i = 1; i < wsize; i++)
   {
      for(j = 1; j < 27; j++)
      {
         strcpy(input, src[i][j]);
         u2 = 0;
         u3 = 0;
         u4 = 0;
         for(u2 = 0; u2 <= 18; u2++)
         {
            if(input[u2] == '_')
            {
               u3 = 1;
            }
            
         }
         if(u3 == 0)
         {
            k9++;
         }
      }
   }

   sprintf(tot_nm, "%d", k9);
   fprintf(fp, "%s\n", tot_nm);

   for(i = 1; i < wsize; i++)
   {
      for(j = 1; j < 27; j++)
      {
         /* p = strstr(src[i][j], "_"); */
         strcpy(input, src[i][j]);
         u2 = 0;
         u4 = 0;
         for(u2 = 0; u2 <= 20; u2++)
         {
            if(input[u2] == '_')
            {
               u4 = 1;
            }
         }
      
         if(u4 == 0)
         {
            
            strcpy(str1, src[i][j]); 
           
            sprintf(strc, "%d", j);
              
            sprintf(strr, "%d", i);
           
            switch(j)
            {
               case 1:
                 if(i < 10)
                 {
                    strcpy(str, " A");
                 }
                 else
                 {  
                    strcpy(str, "A");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 2:
                 if(i < 10)
                 {
                    strcpy(str, " B");
                 }
                 else
                 {  
                    strcpy(str, "B");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 3:
                 if(i < 10)
                 {
                    strcpy(str, " C");
                 }
                 else
                 {  
                    strcpy(str, "C");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 4:
                 if(i < 10)
                 {
                    strcpy(str, " D");
                 }
                 else
                 {  
                    strcpy(str, "D");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 5:
                 if(i < 10)
                 {
                    strcpy(str, " E");
                 }
                 else
                 {  
                    strcpy(str, "E");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 6:
                 if(i < 10)
                 {
                    strcpy(str, " F");
                 }
                 else
                 {  
                    strcpy(str, "F");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 7:
                 if(i < 10)
                 {
                    strcpy(str, " G");
                 }
                 else
                 {  
                    strcpy(str, "G");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 8:
                 if(i < 10)
                 {
                    strcpy(str, " H");
                 }
                 else
                 {  
                    strcpy(str, "H");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 9:
                 if(i < 10)
                 {
                    strcpy(str, " I");
                 }
                 else
                 {  
                    strcpy(str, "I");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 10:
                 if(i < 10)
                 {
                    strcpy(str, " J");
                 }
                 else
                 {  
                    strcpy(str, "J");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 11:
                 if(i < 10)
                 {
                    strcpy(str, " K");
                 }
                 else
                 {  
                    strcpy(str, "K");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 12:
                 if(i < 10)
                 {
                    strcpy(str, " L");
                 }
                 else
                 {  
                    strcpy(str, "L");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 13:
                 if(i < 10)
                 {
                    strcpy(str, " M");
                 }
                 else
                 {  
                    strcpy(str, "M");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 14:
                 if(i < 10)
                 {
                    strcpy(str, " N");
                 }
                 else
                 {  
                    strcpy(str, "N");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 15:
                 if(i < 10)
                 {
                    strcpy(str, " O");
                 }
                 else
                 {  
                    strcpy(str, "O");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 16:
                 if(i < 10)
                 {
                    strcpy(str, " P");
                 }
                 else
                 {  
                    strcpy(str, "P");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 17:
                 if(i < 10)
                 {
                    strcpy(str, " Q");
                 }
                 else
                 {  
                    strcpy(str, "Q");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 18:
                 if(i < 10)
                 {
                    strcpy(str, " R");
                 }
                 else
                 {  
                    strcpy(str, "R");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 19:
                 if(i < 10)
                 {
                    strcpy(str, " S");
                 }
                 else
                 {  
                    strcpy(str, "S");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 20:
                 if(i < 10)
                 {
                    strcpy(str, " T");
                 }
                 else
                 {  
                    strcpy(str, "T");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 21:
                 if(i < 10)
                 {
                    strcpy(str, " U");
                 }
                 else
                 {  
                    strcpy(str, "U");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 22:
                 if(i < 10)
                 {
                    strcpy(str, " V");
                 }
                 else
                 {  
                    strcpy(str, "V");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 23:
                 if(i < 10)
                 {
                    strcpy(str, " W");
                 }
                 else
                 {  
                    strcpy(str, "W");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 24:
                 if(i < 10)
                 {
                    strcpy(str, " X");
                 }
                 else
                 {  
                    strcpy(str, "X");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 25:
                 if(i < 10)
                 {
                    strcpy(str, " Y");
                 }
                 else
                 {  
                    strcpy(str, "Y");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;

               case 26:
                 if(i < 10)
                 {
                    strcpy(str, " Z");
                 }
                 else
                 {  
                    strcpy(str, "Z");
                 }
                 strcat(str, strr);
                 strcat(str, "\0");
                 break;
            }
            fprintf(fp, "%s %s\n", str,str1);
         }
      }     
   }
   fclose(fp);
}


void Load_wsp(void)
{
   int i,j,k,m;
   int lcol;
   int lrow;
   int tot_ct;
   int vv;
   int u2;
   int u5;
   int flag1;

   char str[5];
   char str1[17];
   char w_str1[17];
   char ch;
   char c;
   char wk_num[4];
   char wk_str[4];
 
   Clear_pr();

   EXEC CICS SEND
        MAP("mapc") MAPSET("kkmsd") ERASE
        ;

   EXEC CICS RECEIVE
        MAP("mapc") MAPSET("kkmsd") NOHANDLE
        ;

   strcpy(wk_file, mapc.mapci.lwkspi);
   for(i = 0; i < 9; i++)
   {
      if(isalnum(wk_file[i]))
      {
      }
      else
      {
         wk_file[i] = '\0';
         break;
      }
   }
   strcpy(sheet_nm, "CELL");  
   strcpy(wkspace, wk_file);
         
   strcat(wk_file, " wps A"); 

   fp = fopen(wk_file, "r");
   if(fp == NULL)
   {
         strcpy(mapc.mapco.zerr1o, "File Not found ");
         strcpy(mapc.mapco.zerr2o, "Press Return to Continue");
         strcpy(wkspace, "        ");

         EXEC CICS SEND
              MAP("mapc") MAPSET("kkmsd") ERASE
              ;

         EXEC CICS RECEIVE
              MAP("mapc") MAPSET("kkmsd") NOHANDLE
              ;
      return;
   }


   /*  get record count */
   for(i = 0; i < 4; i++)
   {
      c = fgetc(fp);
      if(c == '\n')
      {
         break;
      }
      wk_num[i] = c;
   }
   wk_num[i] = '\0';
   tot_ct = atoi(wk_num); 
   
   /* source */
   tot_ct--;

   for(vv = 0; vv <= tot_ct; vv++)
   {
      for(k = 0; k < 4; k++)
      {
         c = fgetc(fp);
         if(c == '\n')
         {
            goto end1;
         }
         str[k] = c;
      }
end1:
      str[k] = '\0';

      for(m = 0; m <= 19; m++)
      { 
         c = fgetc(fp);
         if(c == '\n')
         {
            goto end2;
         }
         str1[m] = c;
      }
end2:
      str1[m] = '\0';
      lcol = 0;
      lrow = 0;

      if(str[0] == ' ')
      {
         wk_str[0] = str[1];
         wk_str[1] = '0';
         wk_row[0] = str[2];
         wk_row[1] = '\0';
         lrow = atoi(wk_row);  
      }
      else
      {
         wk_str[0] = str[0];
         wk_str[1] = '\0';
         wk_row[0] = str[1];
         wk_row[1] = str[2];
         wk_row[2] = '\0';
         lrow = atoi(wk_row);  
      }

      for(k = 0; k < 26; k++)
      {
         if(wk_str[0] == alpha[k])
         {
            lcol = k + 1;
         }
      }

       if(str1[0] == '-')
       {
           flag[lrow][lcol] = 1;
           u2 = 0;
           u5 = 0;
           for(u2 = 0; u2 < 19; u2++)
           {
               if(str1[u2] == '.')
               {
                  u5 = 1;
                  break;
               }
           }

           if(u5 == 1)
           {
              spreadsheet[lrow][lcol] = atof(str1);
           }
           else
           {
              spreadsheet[lrow][lcol] = atoi(str1);
           }
       }

       flag1 = 1;
       i = 0;
       for(i = 0; str1[i] != '\0'; i++)
       {
           if((str1[i] >= '0' && str1[i] <= '9') || (str1[i] == '.'))
           {}
           else
           {
               flag1 = 0;
               break;
           } 
       }

       /* if(isNumber(str1))  */
       if(flag1 == 1)   
       {
             u2 = 0;
             u5 = 0;
             for(u2 = 0; u2 < 19; u2++)
             {
                if(str1[u2] == '.')
                {
                   u5 = 1;
                }
             }

             if(u5 == 1)
             {
                spreadsheet[lrow][lcol] = atof(str1);
             }
             else
             {
                spreadsheet[lrow][lcol] = atoi(str1);
             }

             flag[lrow][lcol] = 1;
             strcpy(sheet[lrow][lcol], str1);
             strcpy(src[lrow][lcol], str1);
             if(strlen(sheet[lrow][lcol]) > 12)
             {
                sheet[lrow][lcol][12] = '\0';
                src[lrow][lcol][12] = '\0';
             }
          }
          else                    
          {
             strcpy(sheet[lrow][lcol], str1);
             strcpy(src[lrow][lcol], str1);
             if(strlen(sheet[lrow][lcol]) > 18)
             {
                if(str1[0] != '=')
                {
                   sheet[lrow][lcol][18] = '\0';
                }
                src[lrow][lcol][18] = '\0';
             }
          }
   }
   fclose(fp);
}


void Get_date()
{
    char tmp_var2[2];
    char tmp_var4[4];

    time_t T=time(NULL);
    struct  tm tm = *localtime(&T);
   
    day   = tm.tm_mday;            
    month = tm.tm_mon+1;       
    year  = tm.tm_year+1900; 
 
    sprintf(tmp_var2, "%02d", month);
    strcpy(f_mth, tmp_var2);
    strcpy(f_date, tmp_var2);
    strcat(f_date, "/");
    sprintf(tmp_var2, "%02d",day);
    strcat(f_date, tmp_var2);
    strcpy(f_day, tmp_var2);
    strcat(f_date, "/");
    sprintf(tmp_var4, "%02d",year);
    strcat(f_date, tmp_var4);
    strcpy(f_yr, tmp_var4);
    strcat(f_date, "\0");  
}


void Get_time()
{
    char tmp_var2[2];
    char tmp_var4[4];

    time_t T=time(NULL);
    struct  tm tm = *localtime(&T);

    hour = tm.tm_hour;
    min = tm.tm_min;
    sec = tm.tm_sec;

    sprintf(tmp_var2, "%02d", hour);
    strcpy(f_time, tmp_var2);
    strcat(f_time, ":");
    strcpy(f_hr, tmp_var2);
    sprintf(tmp_var2, "%02d", min);
    strcat(f_time, tmp_var2);
    strcat(f_time , ":");
    strcpy(f_min, tmp_var2);
    sprintf(tmp_var2, "%02d", sec);
    strcat(f_time, tmp_var2);
    strcat(f_time, "\0");
    strcpy(f_sec, tmp_var2);
}



void View_dsp()
{
      
    char wk_col[2];
    char wk_row[3];
          
    Get_date();
    Get_time();

    strcpy(mapd.mapdo.ncolo, " ");
    strcpy(mapd.mapdo.nrowo, " ");

    EXEC CICS SEND
         MAP("mapd") MAPSET("kkmsd") ERASE
         ;

    EXEC CICS RECEIVE
         MAP("mapd") MAPSET("kkmsd") NOHANDLE
         ;

   strcpy(wk_col, mapd.mapdi.ncoli);
   strcpy(wk_row, mapd.mapdi.nrowi);

   z2 = 0;
   for(z1 = 1; z1 <= 27; z1++)
   {
      if(wk_col[0] == sheet[z2][z1][0])
      {
         sv_cct = z1;
         break;
      }
   }
   sv_rct = atoi(wk_row);
}


void Sum_proc()
{
   sum = 0;
   p = strstr(input,"(");
   if(p == NULL)
   {
      
   }
   else
   {
      z1 = 0;
      z4 = 0;
      z2 = 0;
      ch = input[z1];
      while(ch != '(')
      {
         z1++;
         ch = input[z1];
      }
      z1++;
      ch = input[z1];
      while(ch != ',')
      {
         wk_row[z2] = ch;
         z1++;
         z2++;
         ch = input[z1];
      }
      wk_row[z2] = '\0';

      x = strlen(wk_row);

      if(x == 2)
      {
         if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
         {      
                z2 = 0;
                z4 = 1;
                
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(wk_row[0] == sheet[z2][z4][0])
                   {
                      j1 = z4;
                      break;
                   }
                }
              i1 = wk_row[1] - '1'+1;  /* now have row number */

          }
      }

      if(x == 3)
      {
             ch = wk_row[0];

             if((ch >= 'A') && (ch <= 'Z'))
             {
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(ch == sheet[z2][z4][0])
                   {
                      j1 = z4;
                      break;
                   }
                }

             wk_row[0] = wk_row[1];
             wk_row[1] = wk_row[2];
             wk_row[2] = '\0';
             i1 = atoi(wk_row);        /* now have row number */
          }
      }


      z2 = 0;
      z1++;
      ch = input[z1];
      while(ch != ')')
      {
         wk_row[z2] = ch;
         z1++;
         z2++;
         ch = input[z1];
      }
      wk_row[z2] = '\0';

      x = strlen(wk_row);
      if(x == 2)
      {
         if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
         {      
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(wk_row[0] == sheet[z2][z4][0])
                   {
                      j2 = z4;
                      break;
                   }
                }
              i2 = wk_row[1] - '1'+1;  
          }
      }

      if(x == 3)
      {
             if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
             {
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(wk_row[0] == sheet[z2][z4][0])
                   {
                      j2 = z4;
                      break;
                   }
                }

             wk_row[0] = wk_row[1];
             wk_row[1] = wk_row[2];
             wk_row[2] = '\0';
             i2 = atoi(wk_row);       
          }
      }


      if(i2 == i1)
      {
         for(j = j1; j <= j2; j++)
         {
            if(flag[i1][j] == 1)
            {
               sum += spreadsheet[i1][j];
               cnt++;
            }
         }
      }

      if(j2 == j1)
      {
         for(i = i1; i <= i2; i++)
         {
            if(flag[i][j1] == 1)
            {
               sum += spreadsheet[i][j1];
               cnt++;
            }
         }
      }

      sprintf(sheet[i9][jj],"%.2f",sum);
      if(strlen(sheet[i9][jj]) > 12)
      {
         sheet[i9][jj][12] = '\0';
      }
   }
}


void Avg_proc(void)
{
   avg = 0;
   cnt = 0;
   p = strstr(input,"(");
   if(p == NULL)
   {
      
   }
   else 
   {
      z1 = 0;
      z2 = 0;
      z4 = 0;
      ch = input[z1];
      while(ch != '(')
      {
         z1++;
         ch = input[z1];
      }
      z1++;
      ch = input[z1];
      while(ch != ',')
      {
         wk_row[z2] = ch;
         z1++;
         z2++;
         ch = input[z1];
      }
      wk_row[z2] = '\0';

      x = strlen(wk_row);
      if(x == 2)
      {
         if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
         {      
                z2 = 0;
                z4 = 1;
                
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(wk_row[0] == sheet[z2][z4][0])
                   {
                      j1 = z4;
                      break;
                   }
                }
              i1 = wk_row[1] - '1'+1;  /* now have row number */

          }
      }

      if(x == 3)
      {
             ch = wk_row[0];

             if((ch >= 'A') && (ch <= 'Z'))
             {
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(ch == sheet[z2][z4][0])
                   {
                      j1 = z4;
                      break;
                   }
                }

             wk_row[0] = wk_row[1];
             wk_row[1] = wk_row[2];
             wk_row[2] = '\0';
             i1 = atoi(wk_row);        /* now have row number */
          }
      }

       z2 = 0;
       z1++;
       ch = input[z1];
       while(ch != ')')
       {
          wk_row[z2] = ch;
          z1++;
          z2++;
          ch = input[z1];
       }
       wk_row[z2] = '\0';

       x = strlen(wk_row);
      if(x == 2)
      {
         if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
         {      
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(wk_row[0] == sheet[z2][z4][0])
                   {
                      j2 = z4;
                      break;
                   }
                }
              i2 = wk_row[1] - '1'+1;  
          }
      }

      if(x == 3)
      {
             if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
             {
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(wk_row[0] == sheet[z2][z4][0])
                   {
                      j2 = z4;
                      break;
                   }
                }

             wk_row[0] = wk_row[1];
             wk_row[1] = wk_row[2];
             wk_row[2] = '\0';
             i2 = atoi(wk_row);       
          }
      }

      if(i2 == i1)
       {
          for(j = j1; j <= j2; j++)
          {
             if(flag[i1][j] == 1)
             {
                avg += spreadsheet[i1][j];
                cnt++;
             }
          }
  	}

       if(j2 == j1)
       {
          for(i = i1; i <= i2; i++)
          {
             if(flag[i][j1] == 1)
             {
                avg += spreadsheet[i][j1];
                cnt++;
             }
          }
       }

       sprintf(sheet[i9][jj],"%.2f",avg/cnt);
       if(strlen(sheet[i9][jj]) > 12)
       {
          sheet[i9][jj][12] = '\0';
       }
    }
}


void Rng_proc()
{
   p = strstr(input,"(");
   if(p == NULL)
   {
      
   }
   else 
   {
      z1 = 0;
      z2 = 0;
      z4 = 0;
      ch = input[z1];
      while(ch != '(')
      {
         z1++;
         ch = input[z1];
      }
      z1++;
      ch = input[z1];
      while(ch != ',')
      {
         wk_row[z2] = ch;
         z1++;
         z2++;
         ch = input[z1];
      }
      wk_row[z2] = '\0';

      x = strlen(wk_row);
      if(x == 2)
      {
         if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
         {      
                z2 = 0;
                z4 = 1;
                
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(wk_row[0] == sheet[z2][z4][0])
                   {
                      j1 = z4;
                      break;
                   }
                }
              i1 = wk_row[1] - '1'+1;  /* now have row number */

          }
      }

      if(x == 3)
      {
             ch = wk_row[0];

             if((ch >= 'A') && (ch <= 'Z'))
             {
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(ch == sheet[z2][z4][0])
                   {
                      j1 = z4;
                      break;
                   }
                }

             wk_row[0] = wk_row[1];
             wk_row[1] = wk_row[2];
             wk_row[2] = '\0';
             i1 = atoi(wk_row);        /* now have row number */
          }
      }

      z2 = 0;
      z1++;
      ch = input[z1];
      while(ch != ')')
      {
         wk_row[z2] = ch;
         z1++;
         z2++;
         ch = input[z1];
      }
      wk_row[z2] = '\0';

      x = strlen(wk_row);
      x = strlen(wk_row);
      if(x == 2)
      {
         if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
         {      
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(wk_row[0] == sheet[z2][z4][0])
                   {
                      j2 = z4;
                      break;
                   }
                }
              i2 = wk_row[1] - '1'+1;  
          }
      }

      if(x == 3)
      {
             if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
             {
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(wk_row[0] == sheet[z2][z4][0])
                   {
                      j2 = z4;
                      break;
                   }
                }

             wk_row[0] = wk_row[1];
             wk_row[1] = wk_row[2];
             wk_row[2] = '\0';
             i2 = atoi(wk_row);       
          }
      }

      if(i1 == i2)
      {
         for(j = j1; j <= j2; j++)
         {
            if((flag[i1][j] == 1) && (maxm < spreadsheet[i1][j]))
            {
               maxm = spreadsheet[i1][j];
            }
            if((flag[i1][j] == 1) && (minm > spreadsheet[i1][j]))
            {
                minm = spreadsheet[i1][j];
            }
         }
      }

      if(j1 == j2)
      {
         for(i = i1; i <= i2; i++)
         {
            if((flag[i][j1] == 1) && (maxm < spreadsheet[i][j1]))
            {
               maxm = spreadsheet[i][j1];
            }
            if((flag[i][j1] == 1) && (minm > spreadsheet[i][j1]))
            {
                minm = spreadsheet[i][j1];
            }
         }
      }
      spreadsheet[row][col] = maxm-minm;
                     
      sprintf(sheet[i9][jj],"%.2f",avg/cnt);
      if(strlen(sheet[i9][jj]) > 12)
      {
         sheet[i9][jj][12] = '\0';
      }
   }
}



void Date_pr()
{
   Get_date();

   strcpy(sheet[i9][jj], f_date);
   if(strlen(sheet[i9][jj]) > 17)
   {
      sheet[i9][jj][17] = '\0';
   }
}


void Mth_proc()
{
   Get_date();

   sprintf(input,"%d", month);
   spreadsheet[i9][jj] = atoi(input);
   flag[i9][jj] = 1;
   strcpy(sheet[i9][jj],input);
   if(strlen(sheet[i9][jj]) > 12)
   {
      sheet[i9][jj][12] = '\0';
   }
}


void Day_proc()
{
   Get_date();

   sprintf(input,"%d", day);
   spreadsheet[i9][jj] = atoi(input);
   flag[i9][jj] = 1;
   strcpy(sheet[i9][jj],input);
   if(strlen(sheet[i9][jj]) > 12)
   {
     sheet[i9][jj][12] = '\0';
   }
}


void Yr_proc()
{
   Get_date();

   sprintf(input,"%d", year);
   spreadsheet[i9][jj] = atoi(input);
   flag[i9][jj] = 1;
   strcpy(sheet[i9][jj],input);
   if(strlen(sheet[i9][jj]) > 12)
   {
     sheet[i9][jj][12] = '\0';
   }
}


void Time_pr()
{
   Get_time();

   strcpy(sheet[i9][jj], f_time);
   if(strlen(sheet[i9][jj]) > 17)
   {
      sheet[i9][jj][17] = '\0';
   }
}


void Hour_pr()
{
   Get_time();

   sprintf(input,"%d", hour);
   spreadsheet[i9][jj] = atoi(input);
   flag[i9][jj] = 1;
   strcpy(sheet[i9][jj],input);
   if(strlen(sheet[i9][jj]) > 12)
   {
      sheet[i9][jj][12] = '\0';
   }
}


void Min_proc()
{
   Get_time();

   sprintf(input,"%d", min);
   spreadsheet[i9][jj] = atoi(input);
   flag[i9][jj] = 1;
   strcpy(sheet[i9][jj],input);
   if(strlen(sheet[i9][jj]) > 12)
   {
      sheet[i9][jj][12] = '\0';
   }
}


void Sec_proc()
{
   Get_time();

   sprintf(input,"%d", sec);
   spreadsheet[i9][jj] = atoi(input);
   flag[i9][jj] = 1;
   strcpy(sheet[i9][jj],input);
   if(strlen(sheet[i9][jj]) > 12)
   {
      sheet[i9][jj][12] = '\0';
   }
}


void Sqrt_pr()
{
   p = strstr(input,"(");
   if(p == NULL)
   {
      
   }
   else 
   {
      z1 = 0;
      z2 = 0;
      ch = input[z1];
      while(ch != '(')
      {
         z1++;
         ch = input[z1];
      }
      z1++;
      ch = input[z1];
      while(ch != ')')
      {
         wk_row[z2] = ch;
         z1++;
         z2++;
         ch = input[z1];
      }
      wk_row[z2] = '\0';

      x = strlen(wk_row);
      if(x == 2)
      {
         if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
         {      
                z2 = 0;
                z4 = 1;
                
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(wk_row[0] == sheet[z2][z4][0])
                   {
                      j1 = z4;
                      break;
                   }
                }
              i1 = wk_row[1] - '1'+1;  

          }
      }

      if(x == 3)
      {
             ch = wk_row[0];

             if((ch >= 'A') && (ch <= 'Z'))
             {
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(ch == sheet[z2][z4][0])
                   {
                      j1 = z4;
                      break;
                   }
                }

             wk_row[0] = wk_row[1];
             wk_row[1] = wk_row[2];
             wk_row[2] = '\0';
             i1 = atoi(wk_row);        
          }
      }

      value = spreadsheet[i1][j1];
      sum = sqrt(value);
      spreadsheet[i9][jj] = sum;
                     
      sprintf(sheet[i9][jj],"%.2f",sum);
      if(strlen(sheet[i9][jj]) > 12)
      {
         sheet[i9][jj][12] = '\0';
      }
   }
}


void Print_pr()
{
   int st_row;
   int end_row;

   int st_col;
   int end_col;

   sum = 0;

   printf("Enter Starting Cell -> \n");
   scanf("%s",input);

   x = strlen(input);
   
/* printf("print_pr x = %d input = %s\n",x, input); */
   z1 = 0;
   z4 = 0;
   z2 = 0;
   
   if(x == 2)
   {
      wk_row[0] = input[0];
      wk_row[1] = input[1];
      wk_row[2] = '\0';
   }

   if(x == 3)
   {
      wk_row[0] = input[0];
      wk_row[1] = input[1];
      wk_row[2] = input[2];
      wk_row[3] = '\0';
   }

   x = strlen(wk_row);

/* printf("wk_row = %s x = %d\n",wk_row,x); */

   if(x == 2)
   {
      if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
      {      
         z2 = 0;
         z4 = 1;
                
         for(z4 = 1; z4 <= 26; z4++)
         {
            if(wk_row[0] == sheet[z2][z4][0])
            {
               j1 = z4;
               st_col = z4;
               break;
            }
         }
         i1 = wk_row[1] - '1'+1;  /* now have row number */
         st_row = i1;
      }
   }

   if(x == 3)
   {
             ch = wk_row[0];

             if((ch >= 'A') && (ch <= 'Z'))
             {
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(ch == sheet[z2][z4][0])
                   {
                      j1 = z4;
                      st_col = z4;
                      break;
                   }
                }

             wk_row[0] = wk_row[1];
             wk_row[1] = wk_row[2];
             wk_row[2] = '\0';
             i1 = atoi(wk_row);        /* now have row number */
             st_row = i1;
          }
      }
/* printf("st_col = %d st_row = %d\n",st_col,st_row); */

      printf("Enter Ending Cell -> \n");
      scanf("%s",input);

   z1 = 0;
   z4 = 0;
   z2 = 0;
   
   if(x == 2)
   {
      wk_row[0] = input[0];
      wk_row[1] = input[1];
      wk_row[2] = '\0';
   }

   if(x == 3)
   {
      wk_row[0] = input[0];
      wk_row[1] = input[1];
      wk_row[2] = input[2];
      wk_row[3] = '\0';
   }

   x = strlen(wk_row);

/* printf("wk_row = %s x = %d\n",wk_row,x); */

      x = strlen(wk_row);
      if(x == 2)
      {
         if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
         {      
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(wk_row[0] == sheet[z2][z4][0])
                   {
                      j2 = z4;
                      end_col = z4;
                      break;
                   }
                }
              i2 = wk_row[1] - '1'+1; 
              end_row = i2; 
          }
      }

      if(x == 3)
      {
             if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
             {
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(wk_row[0] == sheet[z2][z4][0])
                   {
                      j2 = z4;
                      end_col = z4;
                      break;
                   }
                }

             wk_row[0] = wk_row[1];
             wk_row[1] = wk_row[2];
             wk_row[2] = '\0';
             i2 = atoi(wk_row);  
             end_row = i2;     
          }
      }
 

/* printf("end_col = %d end_row = %d\n",end_col, end_row); */

   fp = fopen("PRINTER", "w");

/*
   fprintf(fp, "%s","This is a test\n");
   fprintf(fp, "%s","LINE TWO\n");
  
   i = 0;
   for(j = cct; j <= cct+4; j++)
   {
      fprintf(fp,"%18s",sheet[i][j]);
   }
   fprintf(fp, "\n\n");

 fprintf(fp, "%s %s %s %d", "We", "are", "in", 2012);

*/

   fclose(fp);

/*
        i = 0;
        for(j = cct; j <= cct+4; j++)
        {
            printf("%18s",sheet[i][j]);
        }
        printf("\n\n");
*/

}


void Pow_dsp()
{
   double xx;

   z3 = 0;
   p = strstr(input,"(");
   if(p == NULL)
   {
     
   }
   else 
   {
      z1 = 0;
      z2 = 0;
      ch = input[z1];
      while(ch != '(')
      {
         z1++;
         ch = input[z1];
      }
      z1++;
      ch = input[z1];
      while(ch != ',')
      {
         wk_row[z2] = ch;
         z1++;
         z2++;
         ch = input[z1];
      }
      wk_row[z2] = '\0';
      z3 = z1;

      x = strlen(wk_row);
      if(x == 2)
      {
         if((wk_row[0] >= 'A') && (wk_row[0] <= 'Z'))
         {      
                z2 = 0;
                z4 = 1;
                
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(wk_row[0] == sheet[z2][z4][0])
                   {
                      j1 = z4;
                      break;
                   }
                }
              i1 = wk_row[1] - '1'+1;  /* now have row number */
          }
      }

      if(x == 3)
      {
             ch = wk_row[0];

             if((ch >= 'A') && (ch <= 'Z'))
             {
                z2 = 0;
                z4 = 1;
                for(z4 = 1; z4 <= 26; z4++)
                {
                   if(ch == sheet[z2][z4][0])
                   {
                      j1 = z4;
                      break;
                   }
                }

             wk_row[0] = wk_row[1];
             wk_row[1] = wk_row[2];
             wk_row[2] = '\0';
             i1 = atoi(wk_row);        /* now have row number */
          }
      }
      value = spreadsheet[i1][j1];

      z2 = 0;
      z1++;
      ch = input[z1];
      while(ch != ')')
      {
         wk_row[z2] = ch;
         z1++;
	  z2++;
         ch = input[z1];
      }
      wk_row[z2] = '\0';
      xx = atof(wk_row);

      sum = pow(value,xx);
      spreadsheet[i9][jj] = sum;
                     
      sprintf(sheet[i9][jj],"%.2f",sum);
      if(strlen(sheet[i9][jj]) > 12)
      {
         sheet[i9][jj][12] = '\0';
      }
   }
}

void Clear_pr()
{
       for(i = 0; i <= wsize; i++)      
       {
          z1 = 1;
          for(j = 0; j <= 26; j++)
          {
             sheet[i][j][0] = '_';
             sheet[i][j][1] = '\0';
             src[i][j][0] = '_';
             src[1][j][1] = '\0'; 

            
             if((i == 0) && (j != 0))
             {
                if(z1 == 10)
                {
                   z1 = 17;
                }
                if(z1 == 26)
                {
                   z1 = 34;
                }
                
                sheet[i][j][0] = 'A'+z1-1;
                src[i][j][0] = 'A'+z1-1;
                z1++;
             }

             if((i != 0) && (j == 0))
             {
                sheet[i][j][0] = '0'+i;
                src[i][j][0] = '0'+i;
             }
             
             if((i == 0) && (j == 0))
             {
                sheet[0][26][0] = 'Z';
                sheet[0][26][1] = '\0';
                src[0][26][0] = 'Z';
                src[0][26][1] = '\0';
             }
          }
       }
      
       for(i = 1; i <= wsize; i++)      
       {
          j = 26;
          {
             sheet[i][j][0] = '_';
             sheet[i][j][1] = '\0';
             src[i][j][0] = '_';
             src[i][j][1] = '\0';
          }
       }

       j = 0;
       for(i = 0; i <= wsize; i++)  		/* row count */    
       {
          if((i != 0) && (j == 0))
          {
             sprintf(sheet[i][j], "%d", i);
             sprintf(src[i][j], "%d", i);
          }
       }

       rct = 0;
       cct = 1;
       sv_rct = rct;
       sv_cct = cct;

}

void Scr_out()
{

     int kk;
     int k2;
     int k3 = 1;
     int ii;
     int j1;
     int cct1;
     int j;
     int clmt;

     char temp[18];
     char drow[2];

     hdr_msg();
     if(exmode == 1)
     {
        strcpy(mapa.mapao.wsheeto, "CELL");
     }
     if(exmode == 2)
     {
        strcpy(mapa.mapao.wsheeto, "PROG");
     }
    
     if(rct == 0)
     {
        rct = 1;
     }

     kk = 0;
     k3 = 1;
     j = 0;
     for(ii = rct; ii <= rct+15; ii++)	/* output row number */
     {
        sprintf(drow, "%s", sheet[ii][j]);
       
        kk++;				
        if(kk == 1)
        {
            strcpy(mapa.mapao.rd1o, drow);
        }
        if(kk == 2)
        {
            strcpy(mapa.mapao.rd2o, drow);
        }
        if(kk == 3)
        {
            strcpy(mapa.mapao.rd3o, drow);
        }
        if(kk == 4)
        {
            strcpy(mapa.mapao.rd4o, drow);
        }
        if(kk == 5)
        {
            strcpy(mapa.mapao.rd5o, drow);
        }
        if(kk == 6)
        {
            strcpy(mapa.mapao.rd6o, drow);
        }
        if(kk == 7)
        {
            strcpy(mapa.mapao.rd7o, drow);
        }
        if(kk == 8)
        {
            strcpy(mapa.mapao.rd8o, drow);
        }
        if(kk == 9)
        {
            strcpy(mapa.mapao.rd9o, drow);
        }
        if(kk == 10)
        {
            strcpy(mapa.mapao.rd10o, drow);
        }
        if(kk == 11)
        {
            strcpy(mapa.mapao.rd11o, drow);
        }
        if(kk == 12)
        {
            strcpy(mapa.mapao.rd12o, drow);
        }
        if(kk == 13)
        {
            strcpy(mapa.mapao.rd13o, drow);
        }
        if(kk == 14)
        {
            strcpy(mapa.mapao.rd14o, drow);
        }
        if(kk == 15)
        {
            strcpy(mapa.mapao.rd15o, drow);
        }

        clmt = 3;
        cct1 = sv_cct;
        if(cct1 > 23)
        {
          cct1 = 23;
        }
                
        k2 = 0;			/* output 4 columns */
        for(j1 = cct1; j1 <= cct1 + clmt; j1++)
        {
            if(exmode == 1)
            {
               sprintf(temp, "%18s", sheet[ii][j1]);
            }

            if(exmode == 2)
            {
               sprintf(temp, "%18s", src[ii][j1]);
            }

            if(k3 == 1)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd1o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd2o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd3o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd4o, temp);
               }
            }

            if(k3 == 2)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd5o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd6o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd7o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd8o, temp);
               }
            }

            if(k3 == 3)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd9o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd10o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd11o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd12o, temp);
               }
            }

           if(k3 == 4)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd13o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd14o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd15o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd16o, temp);
               }
            }

          if(k3 == 5)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd17o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd18o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd19o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd20o, temp);
               }
            }

            if(k3 == 6)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd21o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd22o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd23o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd24o, temp);
               }
            }

            if(k3 == 7)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd25o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd26o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd27o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd28o, temp);
               }
            }

            if(k3 == 8)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd29o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd30o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd31o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd32o, temp);
               }
            }

            if(k3 == 9)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd33o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd34o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd35o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd36o, temp);
               }
            }

            if(k3 == 10)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd37o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd38o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd39o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd40o, temp);
               }
            }

            if(k3 == 11)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd41o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd42o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd43o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd44o, temp);
               }
            }

            if(k3 == 12)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd45o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd46o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd47o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd48o, temp);
               }
            }

            if(k3 == 13)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd49o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd50o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd51o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd52o, temp);
               }
            }

            if(k3 == 14)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd53o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd54o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd55o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd56o, temp);
               }
            }

            if(k3 == 15)
            {
               k2++;
               if(k2 == 1)
               {
                   strcpy(mapa.mapao.bd57o, temp);
               }
               if(k2 == 2)
               {
                   strcpy(mapa.mapao.bd58o, temp);
               }
               if(k2 == 3)
               {
                   strcpy(mapa.mapao.bd59o, temp);
               }
               if(k2 == 4)
               {
                   strcpy(mapa.mapao.bd60o, temp);
               }
            }
        }
        k3++; 	
     }
}

void hdr_msg()
{
     int i5 = 0;
     int j5 = 0;
     int kk = 1;

     Get_time();
     Get_date();

     strcpy(mapa.mapao.stimeo, f_time);
     strcpy(mapa.mapao.sdateo, f_date);
     strcpy(mapa.mapao.wsheeto, "CELL");
     strcpy(mapa.mapao.wspnmo, wkspace); 
     if(cct > 23)
     {
        cct = 23;
     }

     for(j5 = cct; j5 <= cct+3; j5++)
     {
        if(kk == 1)
        {
           strcpy(mapa.mapao.hd1o, sheet[i5][j5]);
        }
        if(kk == 2)
        {
           strcpy(mapa.mapao.hd2o, sheet[i5][j5]);
        }
        if(kk == 3)
        {
           strcpy(mapa.mapao.hd3o, sheet[i5][j5]);
        }
        if(kk == 4)
        {
           strcpy(mapa.mapao.hd4o, sheet[i5][j5]);
        }
        kk++;
     }
}


