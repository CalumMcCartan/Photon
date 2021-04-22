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
    Image img;
} *PImage;

Image* Image_load(const char *fname);
/*
void Image_create(Image *img, int width, int height, int channels, bool zeroed);
void Image_save(const Image *img, const char *fname);
void Image_free(Image *img);
void Image_to_gray(const Image *orig, Image *gray);
void Image_to_sepia(const Image *orig, Image *sepia);
*/
