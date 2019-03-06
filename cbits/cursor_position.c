#include <windows.h>

BOOL set_console_cursor_position(HANDLE h, SHORT x, SHORT y) {
  COORD pos = {x, y};
  SetConsoleCursorPosition(h, pos);
}

BOOL get_console_cursor_position(HANDLE h, SHORT *x, SHORT *y) {
  CONSOLE_CURSOR_INFO a;
  CONSOLE_SCREEN_BUFFER_INFO cbsi;
  if (GetConsoleScreenBufferInfo(h, &cbsi)) {
    COORD pos = cbsi.dwCursorPosition;
    *x = pos.X;
    *y = pos.Y;
    return TRUE;
  } else {
    return FALSE;
  }
}