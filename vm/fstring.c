#include "fstring.h"

/**
 * @brief Return formatted string by string format
 *
 * @param[in] str Buffer for string
 * @param[in] size Size of string buffer
 * @param[in] fmt String format
 * @param[in] args[] Arguments's address
 * @param[in] nu Number of argument
 *
 * @return Length of new string
 */
int fstring(char* str, size_t size, char* fmt, intptr_t args[], int nu)
{
    int old_len = strlen(fmt);
    int buf_len = size;
    int new_len = 0;
    char *fmt_start, *fmt_end, str_fmt[size];
    char f[1024];
    int replace_len = 0;
    fmt_start = fmt;

    while (buf_len > 0 && old_len > 0) {
        switch (*fmt_start) {
        case '%':
            fmt_end = fmt_start;
            fmt_end++;
            while (isdigit(*fmt_end) || *fmt_end == '.') fmt_end++;
            switch (*fmt_end) {
            case 'd':
                memset(f, '\0', 1024);
                strncpy(f, fmt_start, fmt_end - fmt_start + 1);
                replace_len = sprintf(str_fmt, f, *args++);
                if (replace_len < 0) return -1;
                memcpy(str, str_fmt, replace_len);
                str += replace_len;
                buf_len -= replace_len;
                old_len -= (fmt_end - fmt_start + 1);
                new_len += replace_len;
                break;
            case 'c':
                memset(f, '\0', 1024);
                strncpy(f, fmt_start, fmt_end - fmt_start + 1);
                replace_len = sprintf(str_fmt, f, *args++);
                memcpy(str, str_fmt, replace_len);
                str += replace_len;
                buf_len -= replace_len;
                old_len -= (fmt_end - fmt_start + 1);
                new_len += replace_len;
                break;
            case 's':
                memset(f, '\0', 1024);
                strncpy(f, fmt_start, fmt_end - fmt_start + 1);
                replace_len = sprintf(str_fmt, f, (char*)((intptr_t*)*args++));
                if (replace_len < 0) return -1;
                memcpy(str, str_fmt, replace_len);
                str += replace_len;
                buf_len -= replace_len;
                old_len -= (fmt_end - fmt_start + 1);
                new_len += replace_len;
                break;
            case '%':
                str_fmt[0] = '%';
                replace_len = 1;
                memcpy(str, str_fmt, replace_len);
                str += replace_len;
                buf_len -= replace_len;
                old_len -= (fmt_end - fmt_start + 1);
                new_len += replace_len;
                break;
            default:
                printf("Not support print format: %c\n", *fmt_start);
                return -1;
            }
            fmt_start = fmt_end;
            fmt_start++;
            break;
        default:
            *str++ = *fmt_start++;
            new_len++;
            buf_len--;
            old_len--;
            break;
        }
    }
    *str = '\0';
    return new_len;
}
