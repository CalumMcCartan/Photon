// Example of using the Image library in C. Written by Photon team.

#include "Image.h"
#include "utils.h"

int main(void) {
    Image* img1 = Image_load("Shapes.png");
    Image* img2 = Image_load("edwards.png");
    Image* img3 = Image_add(img1, img2);
    Image_save (img3, "addTest.png");
    Image_free(img3);
}
