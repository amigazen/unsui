CC := vc +aosppc
RM := rm -f
CP := cp -p
CFLAGS := -O3 -size -D__USE_INLINE__
LIBS :=

TARGET := mkfile
VERSION := 1
OBJS := mkfile.o

$(TARGET): $(OBJS)
	$(CC) -o $@ $^ $(LIBS)
	protect $@ +e

mkfile.o: $(TARGET)_rev.h

clean:
	$(RM) $(OBJS)

revision:
	bumprev $(VERSION) $(TARGET)
