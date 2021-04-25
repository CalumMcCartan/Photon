#include "Image.h"
#include "utils.h"
#include <math.h>

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image/stb_image.h"
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image/stb_image_write.h"

Image* Image_load(const char *fname) {
    Image* img = malloc(sizeof(Image));
    if((img->data = stbi_load(fname, &img->width, &img->height, &img->channels, 4)) != NULL) {
        img->size = img->width * img->height * img->channels;
        img->allocation_ = STB_ALLOCATED;
    } else {
        printf("Failed to load image %s\n", fname);
    }
    return img;
}

void Image_save(Image* img, const char* fname) {
    if(str_ends_in(fname, ".jpg") || str_ends_in(fname, ".JPG") || str_ends_in(fname, ".jpeg") || str_ends_in(fname, ".JPEG")) {
        stbi_write_jpg(fname, img->width, img->height, img->channels, img->data, 100);
    } else if(str_ends_in(fname, ".png") || str_ends_in(fname, ".PNG")) {
        stbi_write_png(fname, img->width, img->height, img->channels, img->data, img->width * img->channels);
    } else {
        ON_ERROR_EXIT(false, "");
    }
}

int Image_width(Image *img) {
    return img->width;
}

int Image_height(Image *img) {
    return img->height;
}

/*
at the moment, get_pixel returns the value of the first two pixels in the image
*/
int get_pixel(const Image *img, int i) {
    unsigned char *p = img->data;
    unsigned char pixchar = p[i];
    int r = pixchar - 0;
    
    int g = p[i+1] - 0;

    return (int)p[i]; 
}

int set_pixel(Image *img, int pos, int r, int g, int b, int a) {
    unsigned char *p = img -> data;
    unsigned char pixchar = p[pos];

    p[pos] = (char)r;
    p[pos+1] = (char) b;
    p[pos+2] = (char) g;
    p[pos+3] = (char) a;

    return 0;

}

Image* Image_create(int width, int height, uint8_t red, uint8_t green, uint8_t blue, uint8_t alpha) {
    Image* img = malloc(sizeof(Image));
    size_t size = width * height * 4;
    img->data = malloc(size);

    if(img->data != NULL) {
        img->width = width;
        img->height = height;
        img->size = size;
        img->channels = 4;
        img->allocation_ = SELF_ALLOCATED;
    }

    for(unsigned char *p = img->data; p != img->data + img->size; p += img->channels) {
        *p = red;
        *(p + 1) = green;
        *(p + 2) = blue;
        *(p + 3) = alpha;
    }
    return img;
}

void Image_free(Image *img) {
    if(img->allocation_ != NO_ALLOCATION && img->data != NULL) {
        if(img->allocation_ == STB_ALLOCATED) {
            stbi_image_free(img->data);
        } else {
            free(img->data);
        }
        img->data = NULL;
        img->width = 0;
        img->height = 0;
        img->size = 0;
        img->allocation_ = NO_ALLOCATION;
    }
}

Image* Image_to_gray(const Image *orig) {
    //ON_ERROR_EXIT(!(orig->allocation_ != NO_ALLOCATION && orig->channels >= 3), "The input image must have at least 3 channels.");
    int channels = 4;
    Image* gray = Image_create(orig->width, orig->height, 0, 0, 0, 255);
    ON_ERROR_EXIT(gray->data == NULL, "Error in creating the image");
    uint8_t gray_p;

    for(unsigned char *p = orig->data, *pg = gray->data; p != orig->data + orig->size; p += orig->channels, pg += gray->channels) {
        gray_p = (uint8_t)((*p + *(p + 1) + *(p + 2))/3.0);
        *pg = gray_p;
        *(pg + 1) = gray_p;
        *(pg + 2) = gray_p;
        if(orig->channels == 4) {
            *(pg + 3) = *(p + 3);
        }
    }
    return gray;
}