/* Day 3, part 2 */

#include <stdio.h>
#include <string.h>


// Room for 1024 lines of up to 8 digits
#define LINE_WIDTH 16 
#define LINE_MAX 4096
typedef char TLine[LINE_WIDTH];
typedef TLine TData[LINE_MAX];

TData data;

int candidates[LINE_MAX];
int filtered[LINE_MAX];

int zeroes[ LINE_WIDTH ];
int ones[ LINE_WIDTH ];

int tabulate(int count)
{
   // Zero the counts
   for (int digit = 0; digit < LINE_WIDTH; ++digit)
      ones[digit] = zeroes[digit] = 0;

   // Scan each line, counting zeroes and ones.
   int bits = 0;
   for (int i = 0; i < count; ++i)
   {
      char * line = data[candidates[i]];
      for (int digit = 0; line[ digit ]; ++digit)
      {
         switch( line[ digit ] )
         {
         case '0': ++zeroes[digit]; break;
         case '1': ++ones[digit];   break;
         }
         bits = digit+1;
      }
   }

   // bits will be the length of the longest line
   return bits;
}

int cvt(char * bits)
{
   int result = 0;
   for (char * p = bits; *p; ++p)
   {
      int digit = (*p == '1') ? 1 : 0;
      result = (result << 1) | digit;
   }
   return result;
}


int main( int argc, char** argv )
{
   if (argc != 2) return 1;

   FILE * fp = fopen(argv[1], "r");
   if ( !fp ) return 1;

   // Read the data
   int lines = 0;
   int bits = 0;
   while (!feof(fp))
   {
      char * line = data[lines];
      if (!fgets( line, LINE_WIDTH, fp ))
         break;

      char * nl = strchr(line, '\n');
      if (nl) *nl = '\0';

      ++lines;
   }
   fclose( fp );

   int cc;
   // All numbers are candidates at first
   for (cc = 0; cc < lines; ++cc)
      candidates[cc] = cc;
   bits = tabulate(cc);

   // Run through each bit, filtering down
   for (int bit = 0; bit < bits && cc > 1; ++bit)
   {
      // Oxygen takes MOST common digit, ties go to 1
      char filter = (ones[bit] >= zeroes[bit]) ? '1' : '0';
      int fc = 0;
      for (int c = 0; c < cc; ++c)
      {
         int ci = candidates[c];
         if (data[ci][bit] == filter)
            filtered[fc++] = ci;
      }
      printf("Scan bit %d for %c - %d candidates\n",bit, filter, fc);

      // Move filtered into candidates and re-tabulate the ones and zeroes
      for (cc = 0; cc < fc; ++cc)
         candidates[cc] = filtered[cc];
      bits = tabulate(cc);
   }
   char * oxygen = data[candidates[0]];
   int oxy = cvt(oxygen);
   printf("Oxygen = %s (%d)\n", oxygen, oxy);


   // For the scrubber, again consider all lines
   for (cc = 0; cc < lines; ++cc)
      candidates[cc] = cc;
   bits = tabulate(cc);

   for (int bit = 0; bit < bits && cc > 1; ++bit)
   {
      // Scrubber takes LEAST common digit, ties go to 0
      char filter = (ones[bit] < zeroes[bit]) ? '1' : '0';
      int fc = 0;
      for (int c = 0; c < cc; ++c)
      {
         int ci = candidates[c];
         if (data[ci][bit] == filter)
            filtered[fc++] = ci;
      }
      printf("Scan bit %d for %c - %d candidates\n",bit, filter, fc);
      // Move filtered into candidates
      for (cc = 0; cc < fc; ++cc)
         candidates[cc] = filtered[cc];
      bits = tabulate(cc);
   }

   char * scrubber = data[candidates[0]];
   int scrub = cvt(scrubber);
   printf("Scrubber = %s (%d)\n", scrubber, scrub);

   printf("LR Rating = %d\n", scrub * oxy);

   return 0;
}
