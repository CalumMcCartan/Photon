// Example of using the Image library

#include "Image.h"
#include "utils.h"

int main(void) {
    Image* img = Image_load("sky.jpg");
    /*
    ON_ERROR_EXIT(img_sky.data == NULL, "Error in loading the image");
    Image_load(&img_shapes, "Shapes.png");
    ON_ERROR_EXIT(img_shapes.data == NULL, "Error in loading the image");

    // Convert the images to gray
    Image img_sky_gray, img_shapes_gray;
    Image_to_gray(&img_sky, &img_sky_gray);
    Image_to_gray(&img_shapes, &img_shapes_gray);

    // Save images
    Image_save(&img_sky_gray, "sky_gray.jpg");
    Image_save(&img_shapes_gray, "Shapes_gray.png");

    // // Test min max
    // int x, y;
    // x = 4;
    // y = 2;
    // printf("get min: %d\n", get_min(x,y));
    // printf("get max: %d\n", get_max(x,y));
    // printf("get sqrt: %d\n", get_sqrt(x));

    // Release memory
    Image_free(&img_sky);
    Image_free(&img_sky_gray);

    Image_free(&img_shapes);
    Image_free(&img_shapes_gray);
    */
}
