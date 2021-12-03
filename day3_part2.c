/* Day 3, part 2 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Room for 1024 lines of up to 8 digits
#define LINE_WIDTH 16 
#define LINE_MAX 4096

typedef char TEntry[LINE_WIDTH];
typedef TEntry TData[LINE_MAX];

TData data;

// Could dynamically allocate these to match data
int zeroes[LINE_WIDTH];
int ones[LINE_WIDTH];

int candidates[LINE_MAX];

//------------------------------------------------------------------------------

// Count zeroes and ones in the candidate entries
void tabulate(int count)
{
   // Zero the counts
   for (int i = 0; i < LINE_WIDTH; ++i)
      ones[i] = zeroes[i] = 0;

   // Scan each candidate in entry, counting zeroes and ones.
   for (int i = 0; i < count; ++i)
   {
      char * entry = data[candidates[i] ];
      for (int digit = 0; entry[digit]; ++digit)
      {
         switch (entry[digit])
         {
         case '0': ++zeroes[digit]; break;
         case '1': ++ones[digit];   break;
         }
      }
   }
}

//------------------------------------------------------------------------------

// Remove elements of 'candidates' that do not match the criteria.
// Return the number of candidates remaining.
int filter(int count, int digit, char select)
{
   printf("Scan digit %d for %c - ", digit, select);

   int read = 0, write = 0;
   while (read < count)
   {
      int ci = candidates[read++];

      if (data[ci][digit] == select)
         candidates[write++] = ci;
   }

   printf("%d candidates\n", write);

   return write;
}

//------------------------------------------------------------------------------

int main(int argc, char** argv)
{
   if (argc != 2) return 1;

   FILE * fp = fopen(argv[1], "r");
   if (!fp) return 1;

   // -------------------------

   // Read the data
   int lines = 0;
   int digits = 0;
   while (!feof(fp))
   {
      char * entry = data[lines];
      if (!fgets(entry, LINE_WIDTH, fp))
         break;

      // fgets leave the newline. Remove it
      char * nl = strchr(entry, '\n');
      if (nl) *nl = '\0';
      digits = nl - entry;

      ++lines;
   }
   fclose(fp);

   // -------------------------

   int cc; // candidate count

   // All numbers are candidates at first
   for (cc = 0; cc < lines; ++cc)
      candidates[cc] = cc;

   // Run through each digit, filtering down
   for (int digit = 0; digit < digits && cc > 1; ++digit)
   {
      tabulate(cc);
      // Oxygen takes MOST common digit, ties go to 1
      const char select = (ones[digit] >= zeroes[digit]) ? '1' : '0';
      cc = filter(cc, digit, select);
   }

   char * oxygen = data[candidates[0]];
   int oxy = strtoul(oxygen, NULL, 2);
   printf("Oxygen = %s (%d)\n", oxygen, oxy);

   // -------------------------

   // For the scrubber, again consider all lines
   for (cc = 0; cc < lines; ++cc)
      candidates[cc] = cc;

   for (int digit = 0; digit < digits && cc > 1; ++digit)
   {
      tabulate(cc);
      // Scrubber takes LEAST common digit, ties go to 0
      const char select = (ones[digit] < zeroes[digit]) ? '1' : '0';
      cc = filter(cc, digit, select);
   }

   char * scrubber = data[candidates[0]];
   int scrub = strtoul(scrubber, NULL, 2);
   printf("Scrubber = %s (%d)\n", scrubber, scrub);

   printf("LR Rating = %d\n", scrub * oxy);

   return 0;
}

