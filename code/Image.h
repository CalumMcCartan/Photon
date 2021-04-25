/*
Image headers for Photon that uses the stb_image library.
Defines the Image struct, its pointer PImage, the Pixel struct, and its 
PPixel pointer.
struct Image, Image_load, Image_free and Image_save are written by the 
stb_image library team. The rest is written by the Photon team.
*/

#pragma once

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

enum allocation_type {
    NO_ALLOCATION, SELF_ALLOCATED, STB_ALLOCATED
};

typedef struct {
    int width;
    int height;
    int channels;
    size_t size;
    uint8_t *data;
    enum allocation_type allocation_;
} Image;

typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;
} Pixel;

typedef struct {
    Image img;
} *PImage;

typedef struct {
    Pixel pix;
} *PPixel;

Image* Image_load(const char *fname);
void Image_save(Image *img, const char *fname);
Image* Image_create(int width, int height, uint8_t red, uint8_t green, uint8_t blue, uint8_t alpha);
void Image_free(Image *img);
Image* Image_to_gray(const Image *orig);
Image* Image_flip(const Image *orig);
Image* Image_add( Image *img1, Image *img2);
Image* Image_subtract( Image *img1, Image *img2);
Pixel pixel(uint8_t red, uint8_t green, uint8_t blue, uint8_t alpha);
