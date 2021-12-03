/* Day 3 */

#include <stdio.h>

int main( int argc, char** argv )
{
   if (argc != 2) return 1;

   FILE * fp = fopen(argv[1], "r");
   if ( !fp ) return 1;

   int zeroes[32];
   int ones[32];
   for (int i = 0; i < 32; ++i)
   {
      zeroes[i] = ones[i] = 0;
   }

   // Scan data
   int bits = 0;
   while (!feof(fp))
   {
      char line[80];
      if (!fgets( line, sizeof(line), fp ))
         break;

      int digit = 0;
      for (; line[digit] && line[digit] != '\n'; ++digit)
      {
         switch( line[ digit ] )
         {
         case '0': ++zeroes[digit]; break;
         case '1': ++ones[digit];   break;
         }
      }

      bits = digit;
   }
   fclose( fp );

   printf("Bit width=%d\n", bits);
   for (int bit = 0; bit < bits; ++bit)
   {
      printf("bit %d: %d zeroes, %d ones\n", bit, zeroes[bit],ones[bit]);
   }

   int gamma = 0, epsilon = 0;
   for (int i = 0; i < bits; ++i)
   {
      if (ones[i] > zeroes[i])
      {
         gamma |= 1 << (bits - i - 1);
      }
      else
      {
         epsilon |= 1 << (bits - i - 1);
      }
   }
   printf("gamma=%d, epsilon=%d, power=%d",gamma, epsilon, gamma*epsilon);

   return 0;
}
