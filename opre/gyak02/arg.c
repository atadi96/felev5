#include <stdio.h>
#include <stdlib.h>
//call it with some parameters from the command line 

int main(int argc,char ** argv) 
//char** means an array of character arrays = array of strings
{
 if(argc!=3) {
  printf("ERROR: The number of paramters must be 2!");
  return 1;
 }
 int i;
 printf("Number of command line arguments are: %i\n",argc);
 for (i=0;i<argc;i++){
  printf("%i. argument is %s\n",i,argv[i]);
 }

 int c = 0;
 i = 0;
 while(argv[1][i]!=0) {
  c += argv[1][i]=='a';
  ++i;
 }
 printf("Number of 'a' characters in the first parameter: %i\n",c);
 return 0;
}

