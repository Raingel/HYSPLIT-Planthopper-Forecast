/*
 * genctr_test.cpp -- unit tests of classes declared in genctr.h.
 *                    This is based on the Google Test framework.
 *
 * Last revision:
 *  7 JUL 2021 (SYZ) - initial.
 */
#include <gtest/gtest.h>
#include "genctr.h"


class ImageTest : public ::testing::Test {
protected:
   arl::Image<unsigned char> *_p;

public:
   virtual void SetUp() {
      _p = new arl::Image<unsigned char>(10, 7);
   }

   virtual void TearDown() {
      delete _p;
   }
};

TEST_F(ImageTest, ctor) {
   EXPECT_TRUE(_p != NULL);

   EXPECT_EQ(_p->getWidth(), 10);
   EXPECT_EQ(_p->getHeight(), 7);
}

TEST_F(ImageTest, value) {
   EXPECT_TRUE(_p != NULL);

   _p->value(0, 0) = 1;
   EXPECT_EQ(1, _p->value(0,0));

   _p->value(9, 6) = 2;
   EXPECT_EQ(2, _p->value(9,6));
}

TEST_F(ImageTest, clear) {
   EXPECT_TRUE(_p != NULL);

   _p->value(0, 0) = 1;
   _p->clear();
   EXPECT_EQ(0, _p->value(0,0));
}

TEST_F(ImageTest, fill) {
   EXPECT_TRUE(_p != NULL);

   _p->clear();
   _p->fill(5);
   EXPECT_EQ(5, _p->value(0,0));
}


//___________________________________________________________________________

class ConnectedComponentTest : public ::testing::Test {
protected:
   arl::ConnectedComponent *_p;

public:
   virtual void SetUp() {
      _p = new arl::ConnectedComponent();
   }

   virtual void TearDown() {
      delete _p;
   }

   arl::Image<arl::ConnectedComponent::Pixel>* makeImage(unsigned char* data,
         int width, int height) {
      arl::Image<arl::ConnectedComponent::Pixel>* image
            = new arl::Image<arl::ConnectedComponent::Pixel>(width, height);

      for (int j = 0; j < height; j++) {
         unsigned char* row = data + width * j;
         for (int i = 0; i < width; i++) {
            image->value(i, j) = *row++;
         }
      }

      return image;
   }

   arl::Image<arl::ConnectedComponent::Pixel>* makeTestImage1() {
      unsigned char data[] = {
            0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
            0, 1, 1, 0, 1, 1, 0, 1, 1, 1,
            0, 1, 0, 0, 1, 0, 0, 1, 0, 1,
            1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
            1, 0, 0, 0, 0, 1, 0, 1, 0, 1,
            1, 0, 1, 1, 0, 1, 0, 1, 1, 1,
            1, 0, 0, 0, 0, 1, 0, 0, 0, 0,
            1, 1, 1, 1, 1, 1, 0, 1, 1, 1
      };
      int width = 10;
      int height = sizeof(data)/(sizeof(data[0]) * width);
      return makeImage(data, width, height);
   }

   arl::Image<arl::ConnectedComponent::Pixel>* makeTestImage2() {
      unsigned char data[] = {
            0, 0, 0,
            0, 1, 0,
            0, 0, 0
      };
      int width = 3;
      int height = sizeof(data)/(sizeof(data[0]) * width);
      return makeImage(data, width, height);
   }

   arl::Image<arl::ConnectedComponent::Pixel>* makeTestImage3() {
      unsigned char data[] = {
            1, 1, 1,
            1, 0, 1,
            1, 1, 1
      };
      int width = 3;
      int height = sizeof(data)/(sizeof(data[0]) * width);
      return makeImage(data, width, height);
   }
};

TEST_F(ConnectedComponentTest, ctor) {
   EXPECT_TRUE(_p != NULL);

   EXPECT_TRUE(_p->getLabels() == NULL);
   EXPECT_EQ(0, _p->getComponents().size());
   EXPECT_EQ(0, _p->getLabelCount());
}

TEST_F(ConnectedComponentTest, analyze) {
   EXPECT_TRUE(_p != NULL);

   arl::Image<arl::ConnectedComponent::Pixel>* image1 = makeTestImage1();
   _p->analyze(*image1);
   //_p->dump();

   // Components and holes.
   EXPECT_EQ(7, _p->getLabelCount());

   EXPECT_EQ(7, _p->getComponents().size());

   arl::ConnectedComponent::Component* c;

   c = &_p->getComponents().at(0);
   EXPECT_EQ(c->label,  2);
   EXPECT_EQ(c->type,   arl::ConnectedComponent::kComponent);
   EXPECT_EQ(c->i,      5);
   EXPECT_EQ(c->j,      0);

   c = &_p->getComponents().at(1);
   EXPECT_EQ(c->label,  6);
   EXPECT_EQ(c->type,   arl::ConnectedComponent::kComponent);
   EXPECT_EQ(c->i,      7);
   EXPECT_EQ(c->j,      1);

   c = &_p->getComponents().at(2);
   EXPECT_EQ(c->label,  9);
   EXPECT_EQ(c->type,   arl::ConnectedComponent::kHole);

   c = &_p->getComponents().at(3);
   EXPECT_EQ(c->label,  11);
   EXPECT_EQ(c->type,   arl::ConnectedComponent::kHole);

   c = &_p->getComponents().at(4);
   EXPECT_EQ(c->label,  12);
   EXPECT_EQ(c->type,   arl::ConnectedComponent::kHole);

   c = &_p->getComponents().at(5);
   EXPECT_EQ(c->label,  13);
   EXPECT_EQ(c->type,   arl::ConnectedComponent::kComponent);
   EXPECT_EQ(c->i,      2);
   EXPECT_EQ(c->j,      5);

   c = &_p->getComponents().at(6);
   EXPECT_EQ(c->label,  14);
   EXPECT_EQ(c->type,   arl::ConnectedComponent::kComponent);
   EXPECT_EQ(c->i,      7);
   EXPECT_EQ(c->j,      7);

   delete image1;
}


//___________________________________________________________________________

class ChainCodeTest : public ::testing::Test {
protected:
   arl::ChainCode *_p;

public:
   virtual void SetUp() {
      _p = new arl::ChainCode();
   }

   virtual void TearDown() {
      delete _p;
   }
};

TEST_F(ChainCodeTest, ctor) {
   EXPECT_EQ(0, _p->getCount());
}

TEST_F(ChainCodeTest, append) {
   _p->append(arl::ChainCode::kUp);
   _p->append(arl::ChainCode::kLeft);
   _p->append(arl::ChainCode::kUp);
   _p->append(arl::ChainCode::kRight);

   EXPECT_EQ(4, _p->getCount());
   EXPECT_EQ(arl::ChainCode::kUp,      _p->at(0));
   EXPECT_EQ(arl::ChainCode::kLeft,    _p->at(1));
   EXPECT_EQ(arl::ChainCode::kUp,      _p->at(2));
   EXPECT_EQ(arl::ChainCode::kRight,   _p->at(3));
}

TEST_F(ChainCodeTest, append_case2) {
   // list of chain codes over 16 counts.
   int codes[] = {
         0, 1, 2, 3, 2, 1, 3, 2,
         1, 0, 2, 2, 3, 0, 2, 1,
         3, 2, 1, 2, 0, 1, 1, 3
   };
   int codesCount = sizeof(codes) / sizeof(codes[0]);

   for (int k = 0; k < codesCount; k++) {
      _p->append(codes[k]);
   }

   EXPECT_EQ(codesCount, _p->getCount());
   for (int k = 0; k < codesCount; k++) {
      EXPECT_EQ(codes[k], _p->at(k));
   }
}

//___________________________________________________________________________

class ComponentBoundaryTracingTest : public ::testing::Test {
protected:
   arl::ComponentBoundaryTracing *_p;

public:
   virtual void SetUp() {
      _p = new arl::ComponentBoundaryTracing();
   }

   virtual void TearDown() {
      delete _p;
   }
   arl::Image<arl::ConnectedComponent::Pixel>* makeImage(unsigned char* data,
         int width, int height) {
      arl::Image<arl::ConnectedComponent::Pixel>* image
            = new arl::Image<arl::ConnectedComponent::Pixel>(width, height);

      for (int j = 0; j < height; j++) {
         unsigned char* row = data + width * j;
         for (int i = 0; i < width; i++) {
            image->value(i, j) = *row++;
         }
      }

      return image;
   }

   arl::Image<arl::ConnectedComponent::Pixel>* makeTestImage1() {
      unsigned char data[] = {
            0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
            0, 1, 1, 0, 1, 1, 0, 1, 1, 1,
            0, 1, 0, 0, 1, 0, 0, 1, 0, 1,
            1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
            1, 0, 0, 0, 0, 1, 0, 1, 0, 1,
            1, 0, 1, 1, 0, 1, 0, 1, 1, 1,
            1, 0, 0, 0, 0, 1, 0, 0, 0, 0,
            1, 1, 1, 1, 1, 1, 0, 1, 1, 1
      };
      int width = 10;
      int height = sizeof(data)/(sizeof(data[0]) * width);
      return makeImage(data, width, height);
   }

   arl::Image<arl::ConnectedComponent::Pixel>* makeTestImage2() {
      unsigned char data[] = {
            0, 0, 0,
            0, 1, 0,
            0, 0, 0
      };
      int width = 3;
      int height = sizeof(data)/(sizeof(data[0]) * width);
      return makeImage(data, width, height);
   }

   arl::Image<arl::ConnectedComponent::Pixel>* makeTestImage3() {
      unsigned char data[] = {
            1, 1, 1,
            1, 0, 1,
            1, 1, 1
      };
      int width = 3;
      int height = sizeof(data)/(sizeof(data[0]) * width);
      return makeImage(data, width, height);
   }

   arl::ConnectedComponent* makeTestData1() {
      arl::Image<arl::ConnectedComponent::Pixel>* img = makeTestImage1();
      arl::ConnectedComponent* ptr = new arl::ConnectedComponent();
      ptr->analyze(*img);
      delete img;
      return ptr;
   }

   arl::ConnectedComponent* makeTestData2() {
      arl::Image<arl::ConnectedComponent::Pixel>* img = makeTestImage2();
      arl::ConnectedComponent* ptr = new arl::ConnectedComponent();
      ptr->analyze(*img);
      delete img;
      return ptr;
   }

   arl::ConnectedComponent* makeTestData3() {
      arl::Image<arl::ConnectedComponent::Pixel>* img = makeTestImage3();
      arl::ConnectedComponent* ptr = new arl::ConnectedComponent();
      ptr->analyze(*img);
      delete img;
      return ptr;
   }
};

TEST_F(ComponentBoundaryTracingTest, ctor) {
   EXPECT_TRUE(_p != NULL);
}

TEST_F(ComponentBoundaryTracingTest, traceCounterclockwise) {
   EXPECT_TRUE(_p != NULL);

   arl::ConnectedComponent* cc1 = makeTestData1();
   //cc1->dump();

   arl::ChainCode chain;
   arl::ConnectedComponent::Component* c;

   // Components.

   int expected_chain_0[] = {
         1, 0, 1, 1, 0, 0, 3, 2, 3, 0,
         0, 1, 1, 0, 1, 1, 1, 1, 1, 2,
         2, 2, 2, 2, 2, 3, 3, 3, 3, 3,
         0, 3, 2, 3, 3, 0
   };
   c = &cc1->getComponents().at(0);
   EXPECT_EQ(c->type, arl::ConnectedComponent::kComponent);
   _p->traceCounterclockwise(*cc1->getLabels(), c->label, c->i, c->j, chain);
   //chain.dump();
   EXPECT_EQ(36, chain.getCount());
   EXPECT_EQ(c->i, chain.getStartColumn());
   EXPECT_EQ(c->j, chain.getStartRow());
   EXPECT_TRUE(chain.isCounterclockwise());
   for (int k = 0; k < 36; k++) {
      EXPECT_EQ(expected_chain_0[k], chain.at(k));
   }

   int expected_chain_1[] = {
         1, 1, 1, 1, 1, 2, 2, 2, 3, 3,
         3, 3, 3, 0, 0, 0
   };
   c = &cc1->getComponents().at(1);
   EXPECT_EQ(c->type, arl::ConnectedComponent::kComponent);
   _p->traceCounterclockwise(*cc1->getLabels(), c->label, c->i, c->j, chain);
   EXPECT_EQ(16, chain.getCount());
   EXPECT_EQ(c->i, chain.getStartColumn());
   EXPECT_EQ(c->j, chain.getStartRow());
   EXPECT_TRUE(chain.isCounterclockwise());
   for (int k = 0; k < 16; k++) {
      EXPECT_EQ(expected_chain_1[k], chain.at(k));
   }

   int expected_chain_5[] = {
         1, 2, 2, 3, 0, 0
   };
   c = &cc1->getComponents().at(5);
   EXPECT_EQ(c->type, arl::ConnectedComponent::kComponent);
   _p->traceCounterclockwise(*cc1->getLabels(), c->label, c->i, c->j, chain);
   EXPECT_EQ(6, chain.getCount());
   EXPECT_EQ(c->i, chain.getStartColumn());
   EXPECT_EQ(c->j, chain.getStartRow());
   EXPECT_TRUE(chain.isCounterclockwise());
   for (int k = 0; k < 6; k++) {
      EXPECT_EQ(expected_chain_5[k], chain.at(k));
   }

   int expected_chain_6[] = {
         1, 2, 2, 2, 3, 0, 0, 0
   };
   c = &cc1->getComponents().at(6);
   EXPECT_EQ(c->type, arl::ConnectedComponent::kComponent);
   _p->traceCounterclockwise(*cc1->getLabels(), c->label, c->i, c->j, chain);
   EXPECT_EQ(8, chain.getCount());
   EXPECT_EQ(c->i, chain.getStartColumn());
   EXPECT_EQ(c->j, chain.getStartRow());
   EXPECT_TRUE(chain.isCounterclockwise());
   for (int k = 0; k < 8; k++) {
      EXPECT_EQ(expected_chain_6[k], chain.at(k));
   }

   delete cc1;
}

TEST_F(ComponentBoundaryTracingTest, traceClockwise) {
   EXPECT_TRUE(_p != NULL);

   arl::ConnectedComponent* cc1 = makeTestData1();
   //cc1->dump();

   arl::ChainCode chain;
   arl::ConnectedComponent::Component* c;

   // Holes.

   int expected_chain_2[] = {
         2, 1, 0, 3
   };
   c = &cc1->getComponents().at(2);
   EXPECT_EQ(c->type, arl::ConnectedComponent::kHole);
   _p->traceClockwise(*cc1->getLabels(), c->label, c->i, c->j, chain);
   //chain.dump();
   EXPECT_EQ(4, chain.getCount());
   EXPECT_EQ(c->i, chain.getStartColumn());
   EXPECT_EQ(c->j, chain.getStartRow());
   EXPECT_FALSE(chain.isCounterclockwise());
   for (int k = 0; k < 4; k++) {
      EXPECT_EQ(expected_chain_2[k], chain.at(k));
   }

   int expected_chain_3[] = {
         2, 2, 2, 2, 1, 1, 1, 0, 0, 0,
         0, 3, 3, 3
   };
   c = &cc1->getComponents().at(3);
   EXPECT_EQ(c->type, arl::ConnectedComponent::kHole);
   _p->traceClockwise(*cc1->getLabels(), c->label, c->i, c->j, chain);
   //chain.dump();
   EXPECT_EQ(14, chain.getCount());
   EXPECT_EQ(c->i, chain.getStartColumn());
   EXPECT_EQ(c->j, chain.getStartRow());
   EXPECT_FALSE(chain.isCounterclockwise());
   for (int k = 0; k < 14; k++) {
      EXPECT_EQ(expected_chain_3[k], chain.at(k));
   }

   int expected_chain_4[] = {
         2, 1, 0, 3
   };
   c = &cc1->getComponents().at(4);
   EXPECT_EQ(c->type, arl::ConnectedComponent::kHole);
   _p->traceClockwise(*cc1->getLabels(), c->label, c->i, c->j, chain);
   //chain.dump();
   EXPECT_EQ(4, chain.getCount());
   EXPECT_EQ(c->i, chain.getStartColumn());
   EXPECT_EQ(c->j, chain.getStartRow());
   EXPECT_FALSE(chain.isCounterclockwise());
   for (int k = 0; k < 4; k++) {
      EXPECT_EQ(expected_chain_4[k], chain.at(k));
   }

   delete cc1;
}

TEST_F(ComponentBoundaryTracingTest, test_case2) {
   // A component consisting of just one pixel.

   arl::ConnectedComponent* cc1 = makeTestData2();
   //cc1->dump();

   arl::ChainCode chain;
   arl::ConnectedComponent::Component* c;

   // Components.

   int expected_chain_0[] = {
         1, 2, 3, 0
   };
   c = &cc1->getComponents().at(0);
   EXPECT_EQ(c->type, arl::ConnectedComponent::kComponent);
   _p->traceCounterclockwise(*cc1->getLabels(), c->label, c->i, c->j, chain);
   //chain.dump();
   EXPECT_EQ(4, chain.getCount());
   EXPECT_EQ(c->i, chain.getStartColumn());
   EXPECT_EQ(c->j, chain.getStartRow());
   EXPECT_TRUE(chain.isCounterclockwise());
   for (int k = 0; k < 4; k++) {
      EXPECT_EQ(expected_chain_0[k], chain.at(k));
   }

   delete cc1;
}

TEST_F(ComponentBoundaryTracingTest, test_case3) {
   // A component containing a hole.
   // The hole consists of just one pixel.

   arl::ConnectedComponent* cc1 = makeTestData3();
   //cc1->dump();

   arl::ChainCode chain;
   arl::ConnectedComponent::Component* c;

   // Component.
   int expected_chain_0[] = {
         1, 1, 1, 2, 2, 2, 3, 3, 3, 0,
         0, 0
   };
   c = &cc1->getComponents().at(0);
   EXPECT_EQ(c->type, arl::ConnectedComponent::kComponent);
   _p->traceCounterclockwise(*cc1->getLabels(), c->label, c->i, c->j, chain);
   //chain.dump();
   EXPECT_EQ(12, chain.getCount());
   EXPECT_EQ(c->i, chain.getStartColumn());
   EXPECT_EQ(c->j, chain.getStartRow());
   EXPECT_TRUE(chain.isCounterclockwise());
   for (int k = 0; k < 12; k++) {
      EXPECT_EQ(expected_chain_0[k], chain.at(k));
   }

   // Hole.
   int expected_chain_1[] = {
         2, 1, 0, 3
   };
   c = &cc1->getComponents().at(1);
   EXPECT_EQ(c->type, arl::ConnectedComponent::kHole);
   _p->traceClockwise(*cc1->getLabels(), c->label, c->i, c->j, chain);
   //chain.dump();
   EXPECT_EQ(4, chain.getCount());
   EXPECT_EQ(c->i, chain.getStartColumn());
   EXPECT_EQ(c->j, chain.getStartRow());
   EXPECT_FALSE(chain.isCounterclockwise());
   for (int k = 0; k < 4; k++) {
      EXPECT_EQ(expected_chain_1[k], chain.at(k));
   }
   delete cc1;
}

//___________________________________________________________________________

class BoundaryTest : public ::testing::Test {
protected:
   arl::Boundary *_p;

public:
   virtual void SetUp() {
      _p = new arl::Boundary(123);
   }

   virtual void TearDown() {
      delete _p;
   }

};

TEST_F(BoundaryTest, ctor) {
   EXPECT_TRUE(_p != NULL);
   EXPECT_EQ(123, _p->getLabel());
   EXPECT_EQ(0, _p->getHoleCount());
}

//___________________________________________________________________________

class BoundaryTracingTest : public ::testing::Test {
protected:
   arl::BoundaryTracing *_p;

public:
   virtual void SetUp() {
      _p = new arl::BoundaryTracing();
   }

   virtual void TearDown() {
      delete _p;
   }

   arl::ConnectedComponent* makeTestData1() {
       unsigned char data[][10] = {
             {0, 0, 0, 0, 0, 1, 0, 0, 0, 0},
             {0, 1, 1, 0, 1, 1, 0, 1, 1, 1},
             {0, 1, 0, 0, 1, 0, 0, 1, 0, 1},
             {1, 1, 1, 1, 1, 1, 0, 1, 1, 1},
             {1, 0, 0, 0, 0, 1, 0, 1, 0, 1},
             {1, 0, 1, 1, 0, 1, 0, 1, 1, 1},
             {1, 0, 0, 0, 0, 1, 0, 0, 0, 0},
             {1, 1, 1, 1, 1, 1, 0, 1, 1, 1}
       };
       int height = sizeof(data)/sizeof(data[0]);
       int width = sizeof(data[0])/sizeof(data[0][0]);

       arl::Image<arl::ConnectedComponent::Pixel>* img = new arl::Image<arl::ConnectedComponent::Pixel>(width, height);
       for (int j = 0; j < height; j++) {
          for (int i = 0; i < width; i++) {
             img->value(i, j) = data[j][i];
          }
       }

       arl::ConnectedComponent* ptr = new arl::ConnectedComponent();
       ptr->analyze(*img);
       delete img;

       return ptr;
    }
};

TEST_F(BoundaryTracingTest, ctor) {
   EXPECT_TRUE(_p != NULL);
   EXPECT_EQ(0, _p->getBoundaries().size());
}

TEST_F(BoundaryTracingTest, clear) {
   arl::ConnectedComponent* cc = makeTestData1();

   _p->traceBoundaries(*cc->getLabels(), cc->getComponents());

   std::vector<arl::Boundary*>& boundaries = _p->getBoundaries();
   EXPECT_EQ(4, boundaries.size());

   _p->clear();

   // expect no boundaries after calling clear().
   EXPECT_EQ(0, boundaries.size());

   delete cc;
}

TEST_F(BoundaryTracingTest, findEnclosingComponentLabel) {
   arl::ConnectedComponent* cc = makeTestData1();

   arl::ConnectedComponent::Component c;
   arl::ConnectedComponent::Label l;

   // examine the first component.
   c = cc->getComponents().at(0);
   EXPECT_EQ(5, c.i);
   EXPECT_EQ(0, c.j);
   EXPECT_EQ(2, c.label);
   EXPECT_EQ(arl::ConnectedComponent::kComponent, c.type);

   l = _p->findEnclosingComponentLabel(*cc->getLabels(), c);
   EXPECT_EQ(1, l);
   EXPECT_EQ(1, cc->getLabels()->value(c.i - 1, c.j)); // label to the west.

   delete cc;
}

TEST_F(BoundaryTracingTest, traceBoundaries) {
   arl::Boundary* b;
   arl::ConnectedComponent* cc = makeTestData1();

   _p->traceBoundaries(*cc->getLabels(), cc->getComponents());

   // expect 4 boundaries.
   std::vector<arl::Boundary*>& boundaries = _p->getBoundaries();
   EXPECT_EQ(4, boundaries.size());

   // boundary 1 has one hole.
   b = boundaries.at(0);
   EXPECT_EQ(2, b->getLabel());
   EXPECT_EQ(1, b->getHoleCount());

   // boundary 2 has two holes.
   b = boundaries.at(1);
   EXPECT_EQ(6, b->getLabel());
   EXPECT_EQ(2, b->getHoleCount());

   // boundary 3 has no holes.
   b = boundaries.at(2);
   EXPECT_EQ(13, b->getLabel());
   EXPECT_EQ(0, b->getHoleCount());

   // boundary 4 has no holes.
   b = boundaries.at(3);
   EXPECT_EQ(14, b->getLabel());
   EXPECT_EQ(0, b->getHoleCount());

   delete cc;
}

//___________________________________________________________________________

class ContourTest : public ::testing::Test {
protected:
   arl::Contour *_p;

public:
   virtual void SetUp() {
      _p = new arl::Contour();
   }

   virtual void TearDown() {
      delete _p;
   }

};

TEST_F(ContourTest, ctor) {
   EXPECT_TRUE(_p != NULL);
   EXPECT_EQ(0, _p->getHoleCount());
}

//___________________________________________________________________________

class ContourGeneratorTest : public ::testing::Test {
protected:
   arl::ContourGenerator *_p;
   float _epsilon; // tolerance for floating-point number comparison.

public:
   virtual void SetUp() {
      _p = new arl::ContourGenerator();
      _epsilon = 1.0e-4;
   }

   virtual void TearDown() {
      delete _p;
   }

};

TEST_F(ContourGeneratorTest, ctor) {
   EXPECT_TRUE(_p != NULL);
}

TEST_F(ContourGeneratorTest, make) {
   // A simple data array of 3x3.
   float v[] = {
         1.0f, 1.0f, 1.0f,
         1.0f, 2.0f, 1.0f,
         1.0f, 1.0f, 1.0f
   };
   int col = 3;
   int row = 3;

   // Define a chain-code boundary of the above data at contour level 1.5.
   arl::Boundary boundary(1);
   arl::ChainCode* cc = &boundary.getChainCode();
   cc->setStartLocation(1, 1);
   cc->setCounterclockwise(true);
   cc->append(arl::ChainCode::kDown);
   cc->append(arl::ChainCode::kRight);
   cc->append(arl::ChainCode::kUp);
   cc->append(arl::ChainCode::kLeft);

   // Convert the chain-code boundary into a contour at 1.5.
   arl::Contour* contour = _p->make(boundary,
                                    1.5,
                                    v, col, col, row);

   // Verify the contour.
   float expected[][2] = {
         {0.5f, 1.0f},
         {1.0f, 1.5f},
         {1.5f, 1.0f},
         {1.0f, 0.5f},
         {0.5f, 1.0f}
   };
   arl::Contour::PointList& pts = contour->getOuter();
   EXPECT_EQ(5, pts.size());
   for (int k = 0; k < 5; k++) {
      EXPECT_NEAR(expected[k][0], pts[k].x, _epsilon);
      EXPECT_NEAR(expected[k][1], pts[k].y, _epsilon);
   }

   EXPECT_EQ(0, contour->getHoleCount());

   delete contour;
}


TEST_F(ContourGeneratorTest, make_case2) {
   // A simple data array of 11x4.
   // We will trace the contour at 287.0.
   float v[] = {
         287.597f, 287.597f, 292.347f, 293.097f, 293.597f,
         293.097f, 292.097f, 292.597f, 294.597f, 297.097f,
         296.347f,

         288.597f, 286.847f, 285.847f, 286.847f, 289.597f,
         292.597f, 291.097f, 290.597f, 291.597f, 293.097f,
         292.347f,

         292.347f, 289.847f, 286.097f, 285.597f, 285.847f,
         286.597f, 287.097f, 287.847f, 288.347f, 289.847f,
         291.847f,

         293.847f, 292.597f, 290.347f, 289.347f, 287.097f,
         285.347f, 286.097f, 286.597f, 286.847f, 288.097f,
         288.347f
   };
   int col = 11;
   int row = 4;

   float contour_value = 287.0f;

   arl::Image<arl::ConnectedComponent::Pixel> *image =
         new arl::Image<arl::ConnectedComponent::Pixel>(col, row);
   image->clear();
   for (int j = 0; j < row; j++) {
      float* p = v + j * col;
      for (int i = 0; i < col; i++) {
         if (*p >= contour_value) {
            image->value(i, j) = 1;
         }
         p++;
      }
   }

   // Analyze the binary image
   arl::ConnectedComponent cc;
   cc.analyze(*image);
   //cc.dump();

   // Trace boundaries of connected pixels and holes.
   arl::BoundaryTracing boundaryTracer;
   boundaryTracer.traceBoundaries(*cc.getLabels(), cc.getComponents());

   // Construct contour from pixel boundaries.
   std::vector<arl::Boundary*>& boundaries = boundaryTracer.getBoundaries();
   EXPECT_EQ(1, boundaries.size()); // just one boundary.
   arl::Boundary* boundary = boundaries.at(0);
   arl::Contour* contour = _p->make(*boundary,
         contour_value,
         v, col, col, row,
         0.0f);

   // Verify the contour.
   float expected[][2] = {
         {0.0f,      0.0f},
         {0.0f,      1.0f},
         {0.0f,      2.0f},
         {0.0f,      3.0f},
         {0.0f,      3.0f},
         {1.0f,      3.0f},
         {2.0f,      3.0f},
         {3.0f,      3.0f},
         {4.0f,      3.0f},
         {4.05542f,  3.0f},
         {4.0f,      2.92241f},
         {3.0f,      2.37414f},
         {2.0f,      2.21247f},
         {1.7592f,   2.0f},
         {1.0f,      1.05101f},
         {0.912563,  1.0f},
         {1.0f,      0.79598f},
         {2.0f,      0.822613f},
         {3.0f,      0.975518f},
         {3.05564,   1.0f},
         {4.0f,      1.69253f},
         {5.0f,      1.93283f},
         {5.80603f,  2.0f},
         {6.0f,      2.09698f},
         {7.0f,      2.67759f},
         {8.0f,      2.89799f},
         {8.12241,   3.0f},
         {9.0f,      3.0f},
         {10.0f,     3.0f},
         {10.0f,     3.0f},
         {10.0f,     2.0f},
         {10.0f,     1.0f},
         {10.0f,     0.0f},
         {10.0f,     0.0f},
         {9.0f,      0.0f},
         {8.0f,      0.0f},
         {7.0f,      0.0f},
         {6.0f,      0.0f},
         {5.0f,      0.0f},
         {4.0f,      0.0f},
         {3.0f,      0.0f},
         {2.0f,      0.0f},
         {1.0f,      0.0f},
         {0.0f,      0.0f},
   };
   arl::Contour::PointList& pts = contour->getOuter();
   EXPECT_EQ(44, pts.size());
   for (int k = 0; k < 44; k++) {
      EXPECT_NEAR(expected[k][0], pts[k].x, _epsilon);
      EXPECT_NEAR(expected[k][1], pts[k].y, _epsilon);
   }

   EXPECT_EQ(0, contour->getHoleCount());

   delete contour;

   delete image;
}


TEST_F(ContourGeneratorTest, make_case3) {
   // A simple data array of 5x5.
   // We will trace the contour at 302.0.
   float v[] = {
         302.097f, 301.847f, 299.847f, 301.097f, 299.097f,
         300.847f, 300.597f, 302.097f, 302.097f, 300.597f,
         300.097f, 303.097f, 303.347f, 301.347f, 301.847f,
         301.347f, 300.847f, 301.847f, 303.097f, 301.097f,
         299.597f, 299.597f, 300.597f, 300.847f, 300.597f
   };
   int col = 5;
   int row = 5;
   float contour_value = 302.0f;

   arl::Image<arl::ConnectedComponent::Pixel> *image =
         new arl::Image<arl::ConnectedComponent::Pixel>(col, row);
   image->clear();
   for (int j = 0; j < row; j++) {
      float* p = v + j * col;
      for (int i = 0; i < col; i++) {
         if (*p >= contour_value) {
            image->value(i, j) = 1;
         }
         p++;
      }
   }

   // Analyze the binary image
   arl::ConnectedComponent cc;
   cc.analyze(*image);
   //cc.dump();

   // Trace boundaries of connected pixels and holes.
   arl::BoundaryTracing boundaryTracer;
   boundaryTracer.traceBoundaries(*cc.getLabels(), cc.getComponents());

   // Construct contour from pixel boundaries.
   std::vector<arl::Boundary*>& boundaries = boundaryTracer.getBoundaries();
   EXPECT_EQ(3, boundaries.size()); // three boundaries.

   // Contour 1
   arl::Boundary* boundary = boundaries.at(0);
   arl::Contour* contour = _p->make(*boundary,
         contour_value,
         v, col, col, row,
         0.0f);
   float expected1[][2] = {
         {0.0f,      0.0f},
         {0.0f,      0.0775879f},
         {0.387939f, 0.0f},
         {0.0f,      0.0f}
   };
   arl::Contour::PointList* pts = &contour->getOuter();
   EXPECT_EQ(4, pts->size());
   for (int k = 0; k < 4; k++) {
      EXPECT_NEAR(expected1[k][0], pts->at(k).x, _epsilon);
      EXPECT_NEAR(expected1[k][1], pts->at(k).y, _epsilon);
   }
   EXPECT_EQ(0, contour->getHoleCount());
   delete contour;

   // Contour 2
   boundary = boundaries.at(1);
   contour = _p->make(*boundary,
         contour_value,
         v, col, col, row,
         0.0f);
   float expected2[][2] = {
         {1.93534f,  1.0f},
         {1.0f,      1.56121f},
         {0.634338f, 2.0f},
         {1.0f,      2.48755f},
         {2.0f,      2.89799f},
         {2.67349f,  2.0f},
         {3.0f,      1.12931f},
         {3.06466f,  1.0f},
         {3.0f,      0.903015f},
         {2.0f,      0.956896f},
         {1.93534f,  1.0f}
   };
   pts = &contour->getOuter();
   EXPECT_EQ(11, pts->size());
   for (int k = 0; k < 11; k++) {
      EXPECT_NEAR(expected2[k][0], pts->at(k).x, _epsilon);
      EXPECT_NEAR(expected2[k][1], pts->at(k).y, _epsilon);
   }
   EXPECT_EQ(0, contour->getHoleCount());
   delete contour;

   // Contour 3
   boundary = boundaries.at(2);
   contour = _p->make(*boundary,
         contour_value,
         v, col, col, row,
         0.0f);
   float expected3[][2] = {
         {2.12241f,  3.0f},
         {3.0f,      3.48755f},
         {3.54849f,  3.0f},
         {3.0f,      2.37315f},
         {2.12241f,  3.0f}
   };
   pts = &contour->getOuter();
   EXPECT_EQ(5, pts->size());
   for (int k = 0; k < 5; k++) {
      EXPECT_NEAR(expected3[k][0], pts->at(k).x, _epsilon);
      EXPECT_NEAR(expected3[k][1], pts->at(k).y, _epsilon);
   }
   EXPECT_EQ(0, contour->getHoleCount());
   delete contour;

   delete image;
}
