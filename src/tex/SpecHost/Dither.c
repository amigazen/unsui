/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heßmann
*/

#include "Global.h"

STATIC LONG Seed = 42013;

LONG __regargs
ScaleImage(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight)
{
	struct BitMap	*BitMap	= NULL;
	LONG		 Error	= 0;
	UBYTE		*TempLine;

	if(TempLine = CreateTempLine(NewWidth,1))
	{
		if(BitMap = CreateBitMap(NewWidth,NewHeight,1,BMF_CLEAR,NULL))
		{
			struct RastPort __aligned DummyRPort,*TempRPort;

			InitRastPort(&DummyRPort);

			DummyRPort . BitMap = BitMap;

			if(TempRPort = CreateTempRPort(&DummyRPort))
			{
				LONG	 i,j;
				UBYTE	*Line;

				if(NewHeight == Image -> Height)
				{
					if(NewWidth == Image -> Width)
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = Image -> Lines[i];

							for(j = 0 ; j < NewWidth ; j++)
								TempLine[j] = ((BYTE)Filter[*Line++] >= 0);

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}
					else
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = Image -> Lines[i];

							for(j = 0 ; j < NewWidth ; j++)
								TempLine[j] = ((BYTE)Filter[Line[(j * Image -> Width) / NewWidth]] >= 0);

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}
				}
				else
				{
					LONG		Y,LastY = -1;
					PLANEPTR	Offset = BitMap -> Planes[0];

					if(NewWidth == Image -> Width)
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							if((Y = (i * Image -> Height) / NewHeight) == LastY)
								CopyMem(Offset - BitMap -> BytesPerRow,Offset,BitMap -> BytesPerRow);
							else
							{
								Line = Image -> Lines[LastY = Y];

								for(j = 0 ; j < NewWidth ; j++)
									TempLine[j] = ((BYTE)Filter[Line[j]] >= 0);

								(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);
							}

							Offset += BitMap -> BytesPerRow;

							ShowProgress(i);
						}
					}
					else
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							if((Y = (i * Image -> Height) / NewHeight) == LastY)
								CopyMem(Offset - BitMap -> BytesPerRow,Offset,BitMap -> BytesPerRow);
							else
							{
								Line = Image -> Lines[LastY = Y];

								for(j = 0 ; j < NewWidth ; j++)
									TempLine[j] = ((BYTE)Filter[Line[(j * Image -> Width) / NewWidth]] >= 0);

								(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);
							}

							Offset += BitMap -> BytesPerRow;

							ShowProgress(i);
						}
					}
				}

				DeleteTempRPort(TempRPort);
			}
			else
			{
				DeleteBitMap(BitMap);

				BitMap = NULL;

				Error = ERR_NO_MEM;
			}
		}
		else
			Error = ERR_NO_MEM;

		DeleteTempLine(TempLine);
	}
	else
		Error = ERR_NO_MEM;

	*BitMapPtr = BitMap;

	return(Error);
}

LONG __regargs
DitherImage_Matrix(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight,UBYTE *Matrix)
{
	struct BitMap	*BitMap	= NULL;
	LONG		 Error	= 0;
	UBYTE		*TempLine;

	if(TempLine = CreateTempLine(NewWidth,1))
	{
		if(BitMap = CreateBitMap(NewWidth,NewHeight,1,BMF_CLEAR,NULL))
		{
			struct RastPort __aligned DummyRPort,*TempRPort;

			InitRastPort(&DummyRPort);

			DummyRPort . BitMap = BitMap;

			if(TempRPort = CreateTempRPort(&DummyRPort))
			{
				if(Matrix)
				{
					LONG	 i,j;
					UBYTE	*Line,
						*Row = Matrix;

					if(NewHeight == Image -> Height)
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							if(i & 15)
								Row += 16;
							else
								Row = Matrix;

							Line = Image -> Lines[i];

							for(j = 0 ; j < NewWidth ; j++)
								TempLine[j] = (Filter[Line[(j * Image -> Width) / NewWidth]] <= Row[j & 15]);

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}
					else
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							if(i & 15)
								Row += 16;
							else
								Row = Matrix;

							Line = (BYTE *)Image -> Lines[(i * Image -> Height) / NewHeight];

							for(j = 0 ; j < NewWidth ; j++)
								TempLine[j] = (Filter[Line[(j * Image -> Width) / NewWidth]] <= Row[j & 15]);

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}
				}
				else
				{
					UWORD	 Value;
					LONG	 i,j;
					UBYTE	*Line;

					Value = Seed + 16411;

					if(NewHeight == Image -> Height)
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = Image -> Lines[i];

							for(j = 0 ; j < NewWidth ; j++)
							{
								TempLine[j] = (Filter[Line[(j * Image -> Width) / NewWidth]] <= (Value & 0xFF));

								Value += 16411;
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}
					else
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = (BYTE *)Image -> Lines[(i * Image -> Height) / NewHeight];

							for(j = 0 ; j < NewWidth ; j++)
							{
								TempLine[j] = (Filter[Line[(j * Image -> Width) / NewWidth]] <= (Value & 0xFF));

								Value += 16411;
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}

					Seed = Value;
				}

				DeleteTempRPort(TempRPort);
			}
			else
			{
				DeleteBitMap(BitMap);

				BitMap = NULL;

				Error = ERR_NO_MEM;
			}
		}
		else
			Error = ERR_NO_MEM;

		DeleteTempLine(TempLine);
	}
	else
		Error = ERR_NO_MEM;

	*BitMapPtr = BitMap;

	return(Error);
}

LONG __regargs
DitherImage_FS(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight)
{
	struct BitMap	*BitMap	= NULL;
	LONG		 Error	= 0;
	APTR		 Data;

	if(Data = AllocVecPooled(2 * (NewWidth + 2) * sizeof(WORD),MEMF_ANY | MEMF_CLEAR))
	{
		UBYTE *TempLine;

		if(TempLine = CreateTempLine(NewWidth,1))
		{
			if(BitMap = CreateBitMap(NewWidth,NewHeight,1,BMF_CLEAR,NULL))
			{
				struct RastPort __aligned DummyRPort,*TempRPort;

				InitRastPort(&DummyRPort);

				DummyRPort . BitMap = BitMap;

				if(TempRPort = CreateTempRPort(&DummyRPort))
				{
					LONG	 i,j;
					UBYTE	*Line;

					WORD	*ThisError,
						*NextError,
						 Value,
						 Valu2;

					ThisError = Data;
					NextError = ThisError + NewWidth + 2;

					ThisError++;
					NextError++;

					j = Seed + 16411;

					for(i = 0 ; i < NewWidth ; i++)
					{
						ThisError[i] = ((j & 1023) - 512) / 4;

						j += 16411;
					}

					Seed = j;

					if(NewHeight == Image -> Height)
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = Image -> Lines[i];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 16) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Valu2 = Value + Value)
										{
											NextError[j + 1]	 = Value;Value += Valu2;
											NextError[j - 1]	+= Value;Value += Valu2;
											NextError[j]		+= Value;Value += Valu2;
											ThisError[j + 1]	+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Valu2 = Value + Value;

											NextError[j + 1]	 = Value;Value += Valu2;
											NextError[j - 1]	+= Value;Value += Valu2;
											NextError[j]		+= Value;Value += Valu2;
											ThisError[j + 1]	+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= Swap;

									Swap[NewWidth - 1] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 16) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Valu2 = Value + Value)
										{
											NextError[j - 1]	 = Value;Value += Valu2;
											NextError[j + 1]	+= Value;Value += Valu2;
											NextError[j]		+= Value;Value += Valu2;
											ThisError[j - 1]	+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Valu2 = Value + Value;

											NextError[j - 1]	 = Value;Value += Valu2;
											NextError[j + 1]	+= Value;Value += Valu2;
											NextError[j]		+= Value;Value += Valu2;
											ThisError[j - 1]	+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= Swap;

									*Swap = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}
					else
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = (UBYTE *)Image -> Lines[(i * Image -> Height) / NewHeight];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 16) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Valu2 = Value + Value)
										{
											NextError[j + 1]	 = Value;Value += Valu2;
											NextError[j - 1]	+= Value;Value += Valu2;
											NextError[j]		+= Value;Value += Valu2;
											ThisError[j + 1]	+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Valu2 = Value + Value;

											NextError[j + 1]	 = Value;Value += Valu2;
											NextError[j - 1]	+= Value;Value += Valu2;
											NextError[j]		+= Value;Value += Valu2;
											ThisError[j + 1]	+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= Swap;

									Swap[NewWidth - 1] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 16) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Valu2 = Value + Value)
										{
											NextError[j - 1]	 = Value;Value += Valu2;
											NextError[j + 1]	+= Value;Value += Valu2;
											NextError[j]		+= Value;Value += Valu2;
											ThisError[j - 1]	+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Valu2 = Value + Value;

											NextError[j - 1]	 = Value;Value += Valu2;
											NextError[j + 1]	+= Value;Value += Valu2;
											NextError[j]		+= Value;Value += Valu2;
											ThisError[j - 1]	+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= Swap;

									*Swap = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}

					DeleteTempRPort(TempRPort);
				}
				else
				{
					DeleteBitMap(BitMap);

					BitMap = NULL;

					Error = ERR_NO_MEM;
				}
			}
			else
				Error = ERR_NO_MEM;

			DeleteTempLine(TempLine);
		}
		else
			Error = ERR_NO_MEM;

		FreeVecPooled(Data);
	}
	else
		Error = ERR_NO_MEM;

	*BitMapPtr = BitMap;

	return(Error);
}

LONG __regargs
DitherImage_Burkes(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight)
{
	struct BitMap	*BitMap	= NULL;
	LONG		 Error	= 0;
	APTR		 Data;

	if(Data = AllocVecPooled(2 * (NewWidth + 4) * sizeof(WORD),MEMF_ANY | MEMF_CLEAR))
	{
		UBYTE *TempLine;

		if(TempLine = CreateTempLine(NewWidth,1))
		{
			if(BitMap = CreateBitMap(NewWidth,NewHeight,1,BMF_CLEAR,NULL))
			{
				struct RastPort __aligned DummyRPort,*TempRPort;

				InitRastPort(&DummyRPort);

				DummyRPort . BitMap = BitMap;

				if(TempRPort = CreateTempRPort(&DummyRPort))
				{
					LONG	 i,j;
					UBYTE	*Line;

					WORD	*ThisError,
						*NextError,
						 Value;

					ThisError = Data;
					NextError = ThisError + NewWidth + 4;

					ThisError += 2;
					NextError += 2;

					if(NewHeight == Image -> Height)
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = Image -> Lines[i];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 32) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											Value += Value;

											NextError[j + 2]	 = Value;
											NextError[j - 2]	+= Value;Value += Value;
											NextError[j + 1]	+= Value;
											NextError[j - 1]	+= Value;
											ThisError[j + 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j + 1]	+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Value += Value;

											NextError[j + 2]	 = Value;
											NextError[j - 2]	+= Value;Value += Value;
											NextError[j + 1]	+= Value;
											NextError[j - 1]	+= Value;
											ThisError[j + 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j + 1]	+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= Swap;

									Swap[NewWidth - 1] = 0;
									Swap[NewWidth - 2] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 32) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											Value += Value;

											NextError[j - 2]	 = Value;
											NextError[j + 2]	+= Value;Value += Value;
											NextError[j - 1]	+= Value;
											NextError[j + 1]	+= Value;
											ThisError[j - 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j - 1]	+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Value += Value;

											NextError[j - 2]	 = Value;
											NextError[j + 2]	+= Value;Value += Value;
											NextError[j - 1]	+= Value;
											NextError[j + 1]	+= Value;
											ThisError[j - 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j - 1]	+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= Swap;

									Swap[0] = 0;
									Swap[1] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}
					else
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = (UBYTE *)Image -> Lines[(i * Image -> Height) / NewHeight];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 32) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											Value += Value;

											NextError[j + 2]	 = Value;
											NextError[j - 2]	+= Value;Value += Value;
											NextError[j + 1]	+= Value;
											NextError[j - 1]	+= Value;
											ThisError[j + 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j + 1]	+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Value += Value;

											NextError[j + 2]	 = Value;
											NextError[j - 2]	+= Value;Value += Value;
											NextError[j + 1]	+= Value;
											NextError[j - 1]	+= Value;
											ThisError[j + 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j + 1]	+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= Swap;

									Swap[NewWidth - 1] = 0;
									Swap[NewWidth - 2] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 32) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											Value += Value;

											NextError[j - 2]	 = Value;
											NextError[j + 2]	+= Value;Value += Value;
											NextError[j - 1]	+= Value;
											NextError[j + 1]	+= Value;
											ThisError[j - 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j - 1]	+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Value += Value;

											NextError[j - 2]	 = Value;
											NextError[j + 2]	+= Value;Value += Value;
											NextError[j - 1]	+= Value;
											NextError[j + 1]	+= Value;
											ThisError[j - 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j - 1]	+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= Swap;

									Swap[0] = 0;
									Swap[1] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}

					DeleteTempRPort(TempRPort);
				}
				else
				{
					DeleteBitMap(BitMap);

					BitMap = NULL;

					Error = ERR_NO_MEM;
				}
			}
			else
				Error = ERR_NO_MEM;

			DeleteTempLine(TempLine);
		}
		else
			Error = ERR_NO_MEM;

		FreeVecPooled(Data);
	}
	else
		Error = ERR_NO_MEM;

	*BitMapPtr = BitMap;

	return(Error);
}

LONG __regargs
DitherImage_Sierra(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight)
{
	struct BitMap	*BitMap	= NULL;
	LONG		 Error	= 0;
	APTR		 Data;

	if(Data = AllocVecPooled(3 * (NewWidth + 4) * sizeof(WORD),MEMF_ANY | MEMF_CLEAR))
	{
		UBYTE *TempLine;

		if(TempLine = CreateTempLine(NewWidth,1))
		{
			if(BitMap = CreateBitMap(NewWidth,NewHeight,1,BMF_CLEAR,NULL))
			{
				struct RastPort __aligned DummyRPort,*TempRPort;

				InitRastPort(&DummyRPort);

				DummyRPort . BitMap = BitMap;

				if(TempRPort = CreateTempRPort(&DummyRPort))
				{
					LONG	 i,j;
					UBYTE	*Line;

					WORD	*ThisError,
						*NextError,
						*LastError,
						 Fill,
						 Value;

					ThisError = Data;
					NextError = ThisError + NewWidth + 4;
					LastError = NextError + NewWidth + 4;

					ThisError += 2;
					NextError += 2;
					LastError += 2;

					if(NewHeight == Image -> Height)
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = Image -> Lines[i];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 32) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											Fill = Value + Value;

											LastError[j + 1]	 = Fill;
											LastError[j - 1]	+= Fill;
											NextError[j + 2]	+= Fill;
											NextError[j - 2]	+= Fill; Fill += Value;
											ThisError[j + 2]	+= Fill;
											LastError[j]		+= Fill; Fill += Value;
											NextError[j + 1]	+= Fill;
											NextError[j - 1]	+= Fill; Fill += Value;
											NextError[j]		+= Fill;
											ThisError[j + 1]	+= Fill;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Fill = Value + Value;

											LastError[j + 1]	 = Fill;
											LastError[j - 1]	+= Fill;
											NextError[j + 2]	+= Fill;
											NextError[j - 2]	+= Fill; Fill += Value;
											ThisError[j + 2]	+= Fill;
											LastError[j]		+= Fill; Fill += Value;
											NextError[j + 1]	+= Fill;
											NextError[j - 1]	+= Fill; Fill += Value;
											NextError[j]		+= Fill;
											ThisError[j + 1]	+= Fill;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= LastError;
									LastError	= Swap;

									Swap[NewWidth - 1] = 0;
									Swap[NewWidth - 2] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 32) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											Fill = Value + Value;

											LastError[j - 1]	 = Fill;
											LastError[j + 1]	+= Fill;
											NextError[j - 2]	+= Fill;
											NextError[j + 2]	+= Fill; Fill += Value;
											ThisError[j - 2]	+= Fill;
											LastError[j]		+= Fill; Fill += Value;
											NextError[j - 1]	+= Fill;
											NextError[j + 1]	+= Fill; Fill += Value;
											NextError[j]		+= Fill;
											ThisError[j - 1]	+= Fill;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Fill = Value + Value;

											LastError[j - 1]	 = Fill;
											LastError[j + 1]	+= Fill;
											NextError[j - 2]	+= Fill;
											NextError[j + 2]	+= Fill; Fill += Value;
											ThisError[j - 2]	+= Fill;
											LastError[j]		+= Fill; Fill += Value;
											NextError[j - 1]	+= Fill;
											NextError[j + 1]	+= Fill; Fill += Value;
											NextError[j]		+= Fill;
											ThisError[j - 1]	+= Fill;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= LastError;
									LastError	= Swap;

									Swap[0] = 0;
									Swap[1] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}
					else
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = (UBYTE *)Image -> Lines[(i * Image -> Height) / NewHeight];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 32) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											Fill = Value + Value;

											LastError[j + 1]	 = Fill;
											LastError[j - 1]	+= Fill;
											NextError[j + 2]	+= Fill;
											NextError[j - 2]	+= Fill; Fill += Value;
											ThisError[j + 2]	+= Fill;
											LastError[j]		+= Fill; Fill += Value;
											NextError[j + 1]	+= Fill;
											NextError[j - 1]	+= Fill; Fill += Value;
											NextError[j]		+= Fill;
											ThisError[j + 1]	+= Fill;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Fill = Value + Value;

											LastError[j + 1]	 = Fill;
											LastError[j - 1]	+= Fill;
											NextError[j + 2]	+= Fill;
											NextError[j - 2]	+= Fill; Fill += Value;
											ThisError[j + 2]	+= Fill;
											LastError[j]		+= Fill; Fill += Value;
											NextError[j + 1]	+= Fill;
											NextError[j - 1]	+= Fill; Fill += Value;
											NextError[j]		+= Fill;
											ThisError[j + 1]	+= Fill;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= LastError;
									LastError	= Swap;

									Swap[NewWidth - 1] = 0;
									Swap[NewWidth - 2] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 32) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											Fill = Value + Value;

											LastError[j - 1]	 = Fill;
											LastError[j + 1]	+= Fill;
											NextError[j - 2]	+= Fill;
											NextError[j + 2]	+= Fill; Fill += Value;
											ThisError[j - 2]	+= Fill;
											LastError[j]		+= Fill; Fill += Value;
											NextError[j - 1]	+= Fill;
											NextError[j + 1]	+= Fill; Fill += Value;
											NextError[j]		+= Fill;
											ThisError[j - 1]	+= Fill;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Fill = Value + Value;

											LastError[j - 1]	 = Fill;
											LastError[j + 1]	+= Fill;
											NextError[j - 2]	+= Fill;
											NextError[j + 2]	+= Fill; Fill += Value;
											ThisError[j - 2]	+= Fill;
											LastError[j]		+= Fill; Fill += Value;
											NextError[j - 1]	+= Fill;
											NextError[j + 1]	+= Fill; Fill += Value;
											NextError[j]		+= Fill;
											ThisError[j - 1]	+= Fill;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= LastError;
									LastError	= Swap;

									Swap[0] = 0;
									Swap[1] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}

					DeleteTempRPort(TempRPort);
				}
				else
				{
					DeleteBitMap(BitMap);

					BitMap = NULL;

					Error = ERR_NO_MEM;
				}
			}
			else
				Error = ERR_NO_MEM;

			DeleteTempLine(TempLine);
		}
		else
			Error = ERR_NO_MEM;

		FreeVecPooled(Data);
	}
	else
		Error = ERR_NO_MEM;

	*BitMapPtr = BitMap;

	return(Error);
}

LONG __regargs
DitherImage_JJN(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight)
{
	struct BitMap	*BitMap	= NULL;
	LONG		 Error	= 0;
	APTR		 Data;

	if(Data = AllocVecPooled(3 * (NewWidth + 4) * sizeof(WORD),MEMF_ANY | MEMF_CLEAR))
	{
		UBYTE *TempLine;

		if(TempLine = CreateTempLine(NewWidth,1))
		{
			if(BitMap = CreateBitMap(NewWidth,NewHeight,1,BMF_CLEAR,NULL))
			{
				struct RastPort __aligned DummyRPort,*TempRPort;

				InitRastPort(&DummyRPort);

				DummyRPort . BitMap = BitMap;

				if(TempRPort = CreateTempRPort(&DummyRPort))
				{
					LONG	 i,j;
					UBYTE	*Line;

					WORD	*ThisError,
						*NextError,
						*LastError,
						 Fill,
						 Value;

					ThisError = Data;
					NextError = ThisError + NewWidth + 4;
					LastError = NextError + NewWidth + 4;

					ThisError += 2;
					NextError += 2;
					LastError += 2;

					if(NewHeight == Image -> Height)
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = Image -> Lines[i];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 48) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											Fill = Value + Value;

											LastError[j + 2]	 = Value;
											LastError[j - 2]	+= Value;Value += Fill;
											LastError[j + 1]	+= Value;
											LastError[j - 1]	+= Value;
											NextError[j + 2]	+= Value;
											NextError[j - 2]	+= Value;Value += Fill;
											LastError[j]		+= Value;
											NextError[j + 1]	+= Value;
											NextError[j - 1]	+= Value;
											ThisError[j + 2]	+= Value;Value += Fill;
											ThisError[j + 1]	+= Value;
											NextError[j]		+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Fill = Value + Value;

											LastError[j + 2]	 = Value;
											LastError[j - 2]	+= Value;Value += Fill;
											LastError[j + 1]	+= Value;
											LastError[j - 1]	+= Value;
											NextError[j + 2]	+= Value;
											NextError[j - 2]	+= Value;Value += Fill;
											LastError[j]		+= Value;
											NextError[j + 1]	+= Value;
											NextError[j - 1]	+= Value;
											ThisError[j + 2]	+= Value;Value += Fill;
											ThisError[j + 1]	+= Value;
											NextError[j]		+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= LastError;
									LastError	= Swap;

									Swap[NewWidth - 1] = 0;
									Swap[NewWidth - 2] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 48) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											Fill = Value + Value;

											LastError[j - 2]	 = Value;
											LastError[j + 2]	+= Value;Value += Fill;
											LastError[j - 1]	+= Value;
											LastError[j + 1]	+= Value;
											NextError[j - 2]	+= Value;
											NextError[j + 2]	+= Value;Value += Fill;
											LastError[j]		+= Value;
											NextError[j - 1]	+= Value;
											NextError[j + 1]	+= Value;
											ThisError[j - 2]	+= Value;Value += Fill;
											ThisError[j - 1]	+= Value;
											NextError[j]		+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Fill = Value + Value;

											LastError[j - 2]	 = Value;
											LastError[j + 2]	+= Value;Value += Fill;
											LastError[j - 1]	+= Value;
											LastError[j + 1]	+= Value;
											NextError[j - 2]	+= Value;
											NextError[j + 2]	+= Value;Value += Fill;
											LastError[j]		+= Value;
											NextError[j - 1]	+= Value;
											NextError[j + 1]	+= Value;
											ThisError[j - 2]	+= Value;Value += Fill;
											ThisError[j - 1]	+= Value;
											NextError[j]		+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= LastError;
									LastError	= Swap;

									Swap[0] = 0;
									Swap[1] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}
					else
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = (UBYTE *)Image -> Lines[(i * Image -> Height) / NewHeight];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 48) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											Fill = Value + Value;

											LastError[j + 2]	 = Value;
											LastError[j - 2]	+= Value;Value += Fill;
											LastError[j + 1]	+= Value;
											LastError[j - 1]	+= Value;
											NextError[j + 2]	+= Value;
											NextError[j - 2]	+= Value;Value += Fill;
											LastError[j]		+= Value;
											NextError[j + 1]	+= Value;
											NextError[j - 1]	+= Value;
											ThisError[j + 2]	+= Value;Value += Fill;
											ThisError[j + 1]	+= Value;
											NextError[j]		+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Fill = Value + Value;

											LastError[j + 2]	 = Value;
											LastError[j - 2]	+= Value;Value += Fill;
											LastError[j + 1]	+= Value;
											LastError[j - 1]	+= Value;
											NextError[j + 2]	+= Value;
											NextError[j - 2]	+= Value;Value += Fill;
											LastError[j]		+= Value;
											NextError[j + 1]	+= Value;
											NextError[j - 1]	+= Value;
											ThisError[j + 2]	+= Value;Value += Fill;
											ThisError[j + 1]	+= Value;
											NextError[j]		+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= LastError;
									LastError	= Swap;

									Swap[NewWidth - 1] = 0;
									Swap[NewWidth - 2] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 48) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											Fill = Value + Value;

											LastError[j - 2]	 = Value;
											LastError[j + 2]	+= Value;Value += Fill;
											LastError[j - 1]	+= Value;
											LastError[j + 1]	+= Value;
											NextError[j - 2]	+= Value;
											NextError[j + 2]	+= Value;Value += Fill;
											LastError[j]		+= Value;
											NextError[j - 1]	+= Value;
											NextError[j + 1]	+= Value;
											ThisError[j - 2]	+= Value;Value += Fill;
											ThisError[j - 1]	+= Value;
											NextError[j]		+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Fill = Value + Value;

											LastError[j - 2]	 = Value;
											LastError[j + 2]	+= Value;Value += Fill;
											LastError[j - 1]	+= Value;
											LastError[j + 1]	+= Value;
											NextError[j - 2]	+= Value;
											NextError[j + 2]	+= Value;Value += Fill;
											LastError[j]		+= Value;
											NextError[j - 1]	+= Value;
											NextError[j + 1]	+= Value;
											ThisError[j - 2]	+= Value;Value += Fill;
											ThisError[j - 1]	+= Value;
											NextError[j]		+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= LastError;
									LastError	= Swap;

									Swap[0] = 0;
									Swap[1] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}

					DeleteTempRPort(TempRPort);
				}
				else
				{
					DeleteBitMap(BitMap);

					BitMap = NULL;

					Error = ERR_NO_MEM;
				}
			}
			else
				Error = ERR_NO_MEM;

			DeleteTempLine(TempLine);
		}
		else
			Error = ERR_NO_MEM;

		FreeVecPooled(Data);
	}
	else
		Error = ERR_NO_MEM;

	*BitMapPtr = BitMap;

	return(Error);
}

LONG __regargs
DitherImage_Stevenson_Arce(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight)
{
	struct BitMap	*BitMap	= NULL;
	LONG		 Error	= 0;
	APTR		 Data;

	if(Data = AllocVecPooled(4 * (NewWidth + 6) * sizeof(LONG),MEMF_ANY | MEMF_CLEAR))
	{
		UBYTE *TempLine;

		if(TempLine = CreateTempLine(NewWidth,1))
		{
			if(BitMap = CreateBitMap(NewWidth,NewHeight,1,BMF_CLEAR,NULL))
			{
				struct RastPort __aligned DummyRPort,*TempRPort;

				InitRastPort(&DummyRPort);

				DummyRPort . BitMap = BitMap;

				if(TempRPort = CreateTempRPort(&DummyRPort))
				{
					LONG	 i,j;
					UBYTE	*Line;

					LONG	*ThisError,
						*NextError,
						*TempError,
						*LastError,
						 Value;

					ThisError = Data;
					NextError = ThisError + NewWidth + 6;
					TempError = NextError + NewWidth + 6;
					LastError = TempError + NewWidth + 6;

					ThisError += 3;
					NextError += 3;
					TempError += 3;
					LastError += 3;

					if(NewHeight == Image -> Height)
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = Image -> Lines[i];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (LONG)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 200) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											ThisError[j + 2]	+= 32 * Value;

											NextError[j - 3]	+= 12 * Value;
											NextError[j - 1]	+= 26 * Value;
											NextError[j + 1]	+= 30 * Value;
											NextError[j + 3]	+= 16 * Value;

											TempError[j - 2]	+= 12 * Value;
											TempError[j    ]	+= 26 * Value;
											TempError[j + 2]	+= 12 * Value;

											LastError[j - 3]	+=  5 * Value;
											LastError[j - 1]	+= 12 * Value;
											LastError[j + 1]	+= 12 * Value;
											LastError[j + 3]	+=  5 * Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											ThisError[j + 2]	+= 32 * Value;

											NextError[j - 3]	+= 12 * Value;
											NextError[j - 1]	+= 26 * Value;
											NextError[j + 1]	+= 30 * Value;
											NextError[j + 3]	+= 16 * Value;

											TempError[j - 2]	+= 12 * Value;
											TempError[j    ]	+= 26 * Value;
											TempError[j + 2]	+= 12 * Value;

											LastError[j - 3]	+=  5 * Value;
											LastError[j - 1]	+= 12 * Value;
											LastError[j + 1]	+= 12 * Value;
											LastError[j + 3]	+=  5 * Value;
										}
									}
								}

								{
									register LONG *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= TempError;
									TempError	= LastError;
									LastError	= Swap;

									memset(Swap,0,sizeof(LONG) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (LONG)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 200) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											ThisError[j - 2]	+= 32 * Value;

											NextError[j + 3]	+= 12 * Value;
											NextError[j + 1]	+= 26 * Value;
											NextError[j - 1]	+= 30 * Value;
											NextError[j - 3]	+= 16 * Value;

											TempError[j + 2]	+= 12 * Value;
											TempError[j    ]	+= 26 * Value;
											TempError[j - 2]	+= 12 * Value;

											LastError[j + 3]	+=  5 * Value;
											LastError[j + 1]	+= 12 * Value;
											LastError[j - 1]	+= 12 * Value;
											LastError[j - 3]	+=  5 * Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											ThisError[j - 2]	+= 32 * Value;

											NextError[j + 3]	+= 12 * Value;
											NextError[j + 1]	+= 26 * Value;
											NextError[j - 1]	+= 30 * Value;
											NextError[j - 3]	+= 16 * Value;

											TempError[j + 2]	+= 12 * Value;
											TempError[j    ]	+= 26 * Value;
											TempError[j - 2]	+= 12 * Value;

											LastError[j + 3]	+=  5 * Value;
											LastError[j + 1]	+= 12 * Value;
											LastError[j - 1]	+= 12 * Value;
											LastError[j - 3]	+=  5 * Value;
										}
									}
								}

								{
									register LONG *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= TempError;
									TempError	= LastError;
									LastError	= Swap;

									memset(Swap,0,sizeof(LONG) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}
					else
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = (UBYTE *)Image -> Lines[(i * Image -> Height) / NewHeight];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (LONG)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 200) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											ThisError[j + 2]	+= 32 * Value;

											NextError[j - 3]	+= 12 * Value;
											NextError[j - 1]	+= 26 * Value;
											NextError[j + 1]	+= 30 * Value;
											NextError[j + 3]	+= 16 * Value;

											TempError[j - 2]	+= 12 * Value;
											TempError[j    ]	+= 26 * Value;
											TempError[j + 2]	+= 12 * Value;

											LastError[j - 3]	+=  5 * Value;
											LastError[j - 1]	+= 12 * Value;
											LastError[j + 1]	+= 12 * Value;
											LastError[j + 3]	+=  5 * Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											ThisError[j + 2]	+= 32 * Value;

											NextError[j - 3]	+= 12 * Value;
											NextError[j - 1]	+= 26 * Value;
											NextError[j + 1]	+= 30 * Value;
											NextError[j + 3]	+= 16 * Value;

											TempError[j - 2]	+= 12 * Value;
											TempError[j    ]	+= 26 * Value;
											TempError[j + 2]	+= 12 * Value;

											LastError[j - 3]	+=  5 * Value;
											LastError[j - 1]	+= 12 * Value;
											LastError[j + 1]	+= 12 * Value;
											LastError[j + 3]	+=  5 * Value;
										}
									}
								}

								{
									register LONG *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= TempError;
									TempError	= LastError;
									LastError	= Swap;

									memset(Swap,0,sizeof(LONG) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (LONG)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 200) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											ThisError[j - 2]	+= 32 * Value;

											NextError[j + 3]	+= 12 * Value;
											NextError[j + 1]	+= 26 * Value;
											NextError[j - 1]	+= 30 * Value;
											NextError[j - 3]	+= 16 * Value;

											TempError[j + 2]	+= 12 * Value;
											TempError[j    ]	+= 26 * Value;
											TempError[j - 2]	+= 12 * Value;

											LastError[j + 3]	+=  5 * Value;
											LastError[j + 1]	+= 12 * Value;
											LastError[j - 1]	+= 12 * Value;
											LastError[j - 3]	+=  5 * Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											ThisError[j - 2]	+= 32 * Value;

											NextError[j + 3]	+= 12 * Value;
											NextError[j + 1]	+= 26 * Value;
											NextError[j - 1]	+= 30 * Value;
											NextError[j - 3]	+= 16 * Value;

											TempError[j + 2]	+= 12 * Value;
											TempError[j    ]	+= 26 * Value;
											TempError[j - 2]	+= 12 * Value;

											LastError[j + 3]	+=  5 * Value;
											LastError[j + 1]	+= 12 * Value;
											LastError[j - 1]	+= 12 * Value;
											LastError[j - 3]	+=  5 * Value;
										}
									}
								}

								{
									register LONG *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= TempError;
									TempError	= LastError;
									LastError	= Swap;

									memset(Swap,0,sizeof(LONG) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}

					DeleteTempRPort(TempRPort);
				}
				else
				{
					DeleteBitMap(BitMap);

					BitMap = NULL;

					Error = ERR_NO_MEM;
				}
			}
			else
				Error = ERR_NO_MEM;

			DeleteTempLine(TempLine);
		}
		else
			Error = ERR_NO_MEM;

		FreeVecPooled(Data);
	}
	else
		Error = ERR_NO_MEM;

	*BitMapPtr = BitMap;

	return(Error);
}

LONG __regargs
DitherImage_Stucki(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight)
{
	struct BitMap	*BitMap	= NULL;
	LONG		 Error	= 0;
	APTR		 Data;

	if(Data = AllocVecPooled(3 * (NewWidth + 4) * sizeof(WORD),MEMF_ANY | MEMF_CLEAR))
	{
		UBYTE *TempLine;

		if(TempLine = CreateTempLine(NewWidth,1))
		{
			if(BitMap = CreateBitMap(NewWidth,NewHeight,1,BMF_CLEAR,NULL))
			{
				struct RastPort __aligned DummyRPort,*TempRPort;

				InitRastPort(&DummyRPort);

				DummyRPort . BitMap = BitMap;

				if(TempRPort = CreateTempRPort(&DummyRPort))
				{
					LONG	 i,j;
					UBYTE	*Line;

					WORD	*ThisError,
						*NextError,
						*LastError,
						 Value;

					ThisError = Data;
					NextError = ThisError + NewWidth + 4;
					LastError = NextError + NewWidth + 4;

					ThisError += 2;
					NextError += 2;
					LastError += 2;

					if(NewHeight == Image -> Height)
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = Image -> Lines[i];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 42) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											LastError[j + 2]	 = Value;
											LastError[j - 2]	+= Value;Value += Value;
											LastError[j + 1]	+= Value;
											LastError[j - 1]	+= Value;
											NextError[j + 2]	+= Value;
											NextError[j - 2]	+= Value;Value += Value;
											NextError[j + 1]	+= Value;
											NextError[j - 1]	+= Value;
											LastError[j]		+= Value;
											ThisError[j + 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j + 1]	+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											LastError[j + 2]	 = Value;
											LastError[j - 2]	+= Value;Value += Value;
											LastError[j + 1]	+= Value;
											LastError[j - 1]	+= Value;
											NextError[j + 2]	+= Value;
											NextError[j - 2]	+= Value;Value += Value;
											NextError[j + 1]	+= Value;
											NextError[j - 1]	+= Value;
											LastError[j]		+= Value;
											ThisError[j + 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j + 1]	+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= LastError;
									LastError	= Swap;

									Swap[NewWidth - 1] = 0;
									Swap[NewWidth - 2] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 42) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											LastError[j - 2]	 = Value;
											LastError[j + 2]	+= Value;Value += Value;
											LastError[j - 1]	+= Value;
											LastError[j + 1]	+= Value;
											NextError[j - 2]	+= Value;
											NextError[j + 2]	+= Value;Value += Value;
											NextError[j - 1]	+= Value;
											NextError[j + 1]	+= Value;
											LastError[j]		+= Value;
											ThisError[j - 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j - 1]	+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											LastError[j - 2]	 = Value;
											LastError[j + 2]	+= Value;Value += Value;
											LastError[j - 1]	+= Value;
											LastError[j + 1]	+= Value;
											NextError[j - 2]	+= Value;
											NextError[j + 2]	+= Value;Value += Value;
											NextError[j - 1]	+= Value;
											NextError[j + 1]	+= Value;
											LastError[j]		+= Value;
											ThisError[j - 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j - 1]	+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= LastError;
									LastError	= Swap;

									Swap[0] = 0;
									Swap[1] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}
					else
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = (UBYTE *)Image -> Lines[(i * Image -> Height) / NewHeight];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 42) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											LastError[j + 2]	 = Value;
											LastError[j - 2]	+= Value;Value += Value;
											LastError[j + 1]	+= Value;
											LastError[j - 1]	+= Value;
											NextError[j + 2]	+= Value;
											NextError[j - 2]	+= Value;Value += Value;
											NextError[j + 1]	+= Value;
											NextError[j - 1]	+= Value;
											LastError[j]		+= Value;
											ThisError[j + 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j + 1]	+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											LastError[j + 2]	 = Value;
											LastError[j - 2]	+= Value;Value += Value;
											LastError[j + 1]	+= Value;
											LastError[j - 1]	+= Value;
											NextError[j + 2]	+= Value;
											NextError[j - 2]	+= Value;Value += Value;
											NextError[j + 1]	+= Value;
											NextError[j - 1]	+= Value;
											LastError[j]		+= Value;
											ThisError[j + 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j + 1]	+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= LastError;
									LastError	= Swap;

									Swap[NewWidth - 1] = 0;
									Swap[NewWidth - 2] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 42) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										if(Value)
										{
											LastError[j - 2]	 = Value;
											LastError[j + 2]	+= Value;Value += Value;
											LastError[j - 1]	+= Value;
											LastError[j + 1]	+= Value;
											NextError[j - 2]	+= Value;
											NextError[j + 2]	+= Value;Value += Value;
											NextError[j - 1]	+= Value;
											NextError[j + 1]	+= Value;
											LastError[j]		+= Value;
											ThisError[j - 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j - 1]	+= Value;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											LastError[j - 2]	 = Value;
											LastError[j + 2]	+= Value;Value += Value;
											LastError[j - 1]	+= Value;
											LastError[j + 1]	+= Value;
											NextError[j - 2]	+= Value;
											NextError[j + 2]	+= Value;Value += Value;
											NextError[j - 1]	+= Value;
											NextError[j + 1]	+= Value;
											LastError[j]		+= Value;
											ThisError[j - 2]	+= Value;Value += Value;
											NextError[j]		+= Value;
											ThisError[j - 1]	+= Value;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= LastError;
									LastError	= Swap;

									Swap[0] = 0;
									Swap[1] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}

					DeleteTempRPort(TempRPort);
				}
				else
				{
					DeleteBitMap(BitMap);

					BitMap = NULL;

					Error = ERR_NO_MEM;
				}
			}
			else
				Error = ERR_NO_MEM;

			DeleteTempLine(TempLine);
		}
		else
			Error = ERR_NO_MEM;

		FreeVecPooled(Data);
	}
	else
		Error = ERR_NO_MEM;

	*BitMapPtr = BitMap;

	return(Error);
}

LONG __regargs
DitherImage_BlueNoise(struct BitMap **BitMapPtr,struct GreyImage *Image,LONG NewWidth,LONG NewHeight,WORD NoiseLevel)
{
	struct BitMap	*BitMap	= NULL;
	LONG		 Error	= 0;
	APTR		 Data;

	if(Data = AllocVecPooled(2 * (NewWidth + 2) * sizeof(WORD),MEMF_ANY | MEMF_CLEAR))
	{
		UBYTE *TempLine;

		if(TempLine = CreateTempLine(NewWidth,1))
		{
			if(BitMap = CreateBitMap(NewWidth,NewHeight,1,BMF_CLEAR,NULL))
			{
				struct RastPort __aligned DummyRPort,*TempRPort;

				InitRastPort(&DummyRPort);

				DummyRPort . BitMap = BitMap;

				if(TempRPort = CreateTempRPort(&DummyRPort))
				{
					LONG	 i,j;
					UBYTE	*Line;

					WORD	*ThisError,
						*NextError,
						 Value,
						 Valu2,
						 SmallRange,
						 LargeRange;

					SmallRange	= (10 * NoiseLevel) / 100;
					LargeRange	= (50 * NoiseLevel) / 100;

					if(!SmallRange)
						SmallRange = 1;

					if(!LargeRange)
						LargeRange = 1;

					ThisError	= Data;
					NextError	= ThisError + NewWidth + 2;

					ThisError++;
					NextError++;

					j = Seed + 16411;

					for(i = 0 ; i < NewWidth ; i++)
					{
						ThisError[i] = ((j & 1023) - 512) / 4;

						j += 16411;
					}

					Seed = j;

					if(NewHeight == Image -> Height)
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = Image -> Lines[i];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 16) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										Value *= 10;

										if(Valu2 = Value + Value)
										{
											NextError[j + 1]	 = (Value + (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j - 1]	+= (Value - (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j]		+= (Value + (Seed % LargeRange) - 25) / 10;Seed += 16411;Value += Valu2;
											ThisError[j + 1]	+= (Value - (Seed % LargeRange) - 25) / 10;Seed += 16411;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Value *= 10;
											Valu2 = Value + Value;

											NextError[j + 1]	 = (Value + (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j - 1]	+= (Value - (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j]		+= (Value + (Seed % LargeRange) - 25) / 10;Seed += 16411;Value += Valu2;
											ThisError[j + 1]	+= (Value - (Seed % LargeRange) - 25) / 10;Seed += 16411;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= Swap;

									Swap[NewWidth - 1] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 16) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										Value *= 10;

										if(Valu2 = Value + Value)
										{
											NextError[j - 1]	 = (Value + (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j + 1]	+= (Value - (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j]		+= (Value + (Seed % LargeRange) - 25) / 10;Seed += 16411;Value += Valu2;
											ThisError[j - 1]	+= (Value - (Seed % LargeRange) - 25) / 10;Seed += 16411;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Value *= 10;

											NextError[j - 1]	 = (Value + (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j + 1]	+= (Value - (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j]		+= (Value + (Seed % LargeRange) - 25) / 10;Seed += 16411;Value += Valu2;
											ThisError[j - 1]	+= (Value - (Seed % LargeRange) - 25) / 10;Seed += 16411;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= Swap;

									*Swap = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}
					else
					{
						for(i = 0 ; i < NewHeight ; i++)
						{
							Line = (UBYTE *)Image -> Lines[(i * Image -> Height) / NewHeight];

							if(i & 1)
							{
								for(j = 0 ; j < NewWidth ; j++)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 16) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										Value *= 10;

										if(Valu2 = Value + Value)
										{
											NextError[j + 1]	 = (Value + (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j - 1]	+= (Value - (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j]		+= (Value + (Seed % LargeRange) - 25) / 10;Seed += 16411;Value += Valu2;
											ThisError[j + 1]	+= (Value - (Seed % LargeRange) - 25) / 10;Seed += 16411;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Value *= 10;
											Valu2 = Value + Value;

											NextError[j + 1]	 = (Value + (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j - 1]	+= (Value - (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j]		+= (Value + (Seed % LargeRange) - 25) / 10;Seed += 16411;Value += Valu2;
											ThisError[j + 1]	+= (Value - (Seed % LargeRange) - 25) / 10;Seed += 16411;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= Swap;

									Swap[NewWidth - 1] = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}
							else
							{
								for(j = NewWidth - 1 ; j >= 0 ; j--)
								{
									if((Value = (WORD)Filter[Line[(j * Image -> Width) / NewWidth]] + ThisError[j] / 16) > 255)
										Value = 255;
									else
									{
										if(Value < 0)
											Value = 0;
									}

									if(TempLine[j] = (Value < 128))
									{
										Value *= 10;

										if(Valu2 = Value + Value)
										{
											NextError[j - 1]	 = (Value + (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j + 1]	+= (Value - (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j]		+= (Value + (Seed % LargeRange) - 25) / 10;Seed += 16411;Value += Valu2;
											ThisError[j - 1]	+= (Value - (Seed % LargeRange) - 25) / 10;Seed += 16411;
										}
									}
									else
									{
										if(Value = Value - 255)
										{
											Value *= 10;
											Valu2 = Value + Value;

											NextError[j - 1]	 = (Value + (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j + 1]	+= (Value - (Seed % SmallRange) -  5) / 10;Seed += 16411;Value += Valu2;
											NextError[j]		+= (Value + (Seed % LargeRange) - 25) / 10;Seed += 16411;Value += Valu2;
											ThisError[j - 1]	+= (Value - (Seed % LargeRange) - 25) / 10;Seed += 16411;
										}
									}
								}

								{
									register WORD *Swap;

									Swap		= ThisError;
									ThisError	= NextError;
									NextError	= Swap;

									*Swap = 0;

									memset(Swap,0,sizeof(WORD) * NewWidth);
								}
							}

							(* WriteLine)(&DummyRPort,i,NewWidth,TempLine,TempRPort);

							ShowProgress(i);
						}
					}

					DeleteTempRPort(TempRPort);
				}
				else
				{
					DeleteBitMap(BitMap);

					BitMap = NULL;

					Error = ERR_NO_MEM;
				}
			}
			else
				Error = ERR_NO_MEM;

			DeleteTempLine(TempLine);
		}
		else
			Error = ERR_NO_MEM;

		FreeVecPooled(Data);
	}
	else
		Error = ERR_NO_MEM;

	*BitMapPtr = BitMap;

	return(Error);
}
