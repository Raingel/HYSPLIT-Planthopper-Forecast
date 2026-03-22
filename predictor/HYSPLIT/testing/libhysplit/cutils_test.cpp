/*
 * cutils_test.cpp -- unit tests of classes declared in cutils.h.
 *                    This is based on the Google Test framework.
 *
 * Last revision:
 * 27 JUL 2021 (SYZ) - initial.
 */
#include <gtest/gtest.h>
#include "cutils.h"


TEST(CUtilsTest, fortranStringLength) {
   EXPECT_EQ(4, arl::fortranStringLength((unsigned char *) "TEST    ", 8));
   EXPECT_EQ(4, arl::fortranStringLength((unsigned char *) "TEST", 4));
   EXPECT_EQ(2, arl::fortranStringLength((unsigned char *) "TEST", 2));
}

TEST(CUtilsTest, fortranStringCopy) {
   unsigned char *s1 = (unsigned char *) "TEST    "; // 8 chars
   char buf[80];
   int len;

   len = arl::fortranStringCopy(s1, 8, (unsigned char *) buf, 80);
   EXPECT_EQ(4, len);
   EXPECT_STREQ("TEST", buf);

   // Only three characters are copied to include a null-termination character at the end.
   len = arl::fortranStringCopy(s1, 8, (unsigned char *) buf, 4);
   EXPECT_EQ(3, len);
   EXPECT_STREQ("TES", buf);
}
