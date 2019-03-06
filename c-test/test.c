#include <windows.h>
#include <stdio.h>

#if TRUE

int main(VOID) 
{ 
    HANDLE hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);

    DWORD numCharsWritten;
    WriteConsole(hStdOut, TEXT("test\n"), 5, &numCharsWritten, NULL);

    CONSOLE_FONT_INFOEX fontInfo;
    GetCurrentConsoleFontEx(hStdOut, FALSE, &fontInfo);

    fontInfo.FontWeight = 700;
    fontInfo.dwFontSize.Y = 10;
    SetCurrentConsoleFontEx(hStdOut, FALSE, &fontInfo);
  
    WriteConsole(hStdOut, TEXT("test\n"), 5, &numCharsWritten, NULL);
    return 0;
}

#else
int main() {
  printf("Sizeof: %d\n", sizeof(SMALL_RECT));
  printf("Alignof: %d\n", _Alignof(SMALL_RECT));

  return 0;
}
#endif