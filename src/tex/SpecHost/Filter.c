/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heßmann
*/

#include "Global.h"

VOID __regargs
BrightnessFilter(UBYTE *Table,LONG Scale)
{
	if(Scale == -100)
		memset(Table,0,256);
	else
	{
		LONG i;

		if(Scale != 0)
		{
			LONG Value;

			if(Scale > 0)
			{
				Scale = (255 * Scale) / 100;
	
				for(i = 0 ; i < 256 ; i++)
				{
					if((Value = ((LONG)Table[i]) + Scale) >= 255)
						Table[i] = 255;
					else
						Table[i] = Value;
				}
			}
			else
			{
				Scale = (255 * (-Scale)) / 100;
	
				for(i = 0 ; i < 256 ; i++)
				{
					if((Value = ((LONG)Table[i]) - Scale) <= 0)
						Table[i] = 0;
					else
						Table[i] = Value;
				}
			}
		}
	}
}

VOID __regargs
ContrastFilter(UBYTE *Table,LONG Scale)
{
	if(Scale != 0)
	{
		LONG Delta,i;

		if(Scale < 0)
		{
			Scale += 100;
	
			for(i = 0 ; i < 128 ; i++)
			{
				if((Delta = ((127 - (LONG)Table[i]) * Scale) / 100) > 127)
					Delta = 127;
				else
				{
					if(Delta < 0)
						Delta = 0;
				}
	
				Table[i] = 127 - Delta;
	
				if((Delta = (((LONG)Table[i + 128] - 128) * Scale) / 100) > 127)
					Delta = 127;
				else
				{
					if(Delta < 0)
						Delta = 0;
				}
	
				Table[i + 128] = 128 + Delta;
			}
		}
		else
		{
			LONG Greatest,Smallest;

			if((Greatest = (LONG)Table[255 - (127 * Scale) / 100] - 128) < 1)
				Greatest = 1;

			if((Smallest = 127 - (127 * Scale) / 100) < 1)
				Smallest = 1;

			for(i = 0 ; i < 128 ; i++)
			{
				if((Delta = (128 * (127 - (LONG)Table[i])) / Smallest) > 127)
					Delta = 127;
				else
				{
					if(Delta < -127)
						Delta = -127;
				}

				Table[i] = 127 - Delta;

				if((Delta = (128 * ((LONG)Table[i + 128] - 128)) / Greatest) > 127)
					Delta = 127;
				else
				{
					if(Delta < -127)
						Delta = -127;
				}
	
				Table[i + 128] = 128 + Delta;
			}
		}
	}
}

VOID __regargs
GammaFilter(UBYTE *Table,LONG Scale)
{
	if(Scale == -100)
		memset(Table,0,256);
	else
	{
		if(Scale != 0)
		{
			register float	Gamma,Max;
			register LONG	Value,i;

			Gamma	= 100.0 / (float)(Scale + 100);
			Max	= 255.0;

			for(i = 0 ; i < 256 ; i++)
			{
				if((Value = (LONG)(Max * pow((float)Table[i] / Max,Gamma) + 0.5)) >= 255)
					Table[i] = 255;
				else
					Table[i] = Value;
			}
		}
	}
}
