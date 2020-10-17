#include "AdderScan.h"

long long AdderScan_IntegerFast(const char *str, int *pLen)
{
  const char *start = str;
  long long val = 0;
  int neg = 0;
  while ((str[0] < '0' || str[0] > '9') && !(*str)) ++str;

  if (!str[0])
    return 0;

  if (*(str - 1) == '-')
  {
    neg = 1;
    ++str;
  }

  for (; str[0] >= '0' && str[0] <= '9'; ++str)
    val = (val << 1) + (val << 3) + str[0] - '0';

  if (pLen)
    *pLen = (int)(str - start);

  return neg ? -val : val;
}

long long AdderScan_HexIntegerFast(const char *str, int *pLen)
{
  const char *start = str;
  long long val = 0;
  int neg = 0;
  while (!((str[0] >= '0' && str[0] <= '9') || (str[0] >= 'A' || str[0] <= 'F')) && !(*str)) ++str;

  if (!str[0])
    return 0;

  if (*(str - 1) == '-')
  {
    neg = 1;
    ++str;
  }

  for (; (str[0] >= '0' && str[0] <= '9') || (str[0] >= 'A' && str[0] <= 'F'); ++str)
  {
    int offset = ((str[0] >= '0' && str[0] <= '9') ? '0' : ('A' - 10));
    val = (val << 4ll) + str[0] - offset;
  }

  if (pLen)
    *pLen = (int)(str - start);

  return neg ? -val : val;
}
