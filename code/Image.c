/*
Image library for Photon that implements the stb_image library.
Image_load, Image_free and Image_save are written by the stb_image 
library team, the rest is written by the Photon team.

Authors:
Franky Campuzano (fc2608)
Akira Higaki (abh2171)
Calum McCartan (cm4114)
Phu D Pham (pdp2121)
*/

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

int get_position(int width, int channels, int x, int y) {
    return (x + (width * y)) * (channels); 
}

Pixel get_pixel(const Image *img, int x, int y) {
    int pos = get_position(img->width, img-> channels, x,y);
    unsigned char *p = img->data;
    unsigned char pixchar = p[pos];

    return pixel(p[pos], p[pos + 1], p[pos + 2], p[pos + 3]); 
}

int set_pixel(Image *img, int x, int y, Pixel pix) {
    unsigned char *p = img -> data;
    int pos = get_position(img->width, img-> channels, x,y);

    p[pos] = pix.r;
    p[pos+1] = pix.g;
    p[pos+2] = pix.b;
    p[pos+3] = pix.a;

    return 0;
}

Pixel pixel(uint8_t red, uint8_t green, uint8_t blue, uint8_t alpha) {
    Pixel p;
    p.r = red;
    p.g = green;
    p.b = blue;
    p.a = alpha;
    return p;
}

uint8_t pixel_attr(Pixel p, int attr) {
    switch(attr) {
        case 0: return p.r;
        case 1: return p.g;
        case 2: return p.b;
        case 3: return p.a;
        default: printf("Internal error: pixel has no %d attr", attr);
    }
    return 0;
}

Image* Image_create(int width, int height, Pixel col) {
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
        *p = col.r;
        *(p + 1) = col.g;
        *(p + 2) = col.b;
        *(p + 3) = col.a;
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

Image* Image_paste( Image *gray, const Image *orig, int x, int y ) {
    //ON_ERROR_EXIT(!(orig->allocation_ != NO_ALLOCATION && orig->channels >= 3), "The input image must have at least 3 channels.");
    
    int pos = get_position(gray->width, gray-> channels, x,y);

    int i = 0;
    bool fixedpos = false;
    for(unsigned char *p = orig->data, *pg = gray->data; p != orig->data + orig->size; p += orig->channels, pg += gray->channels) {
        
        if(!fixedpos) {
            pg+=pos;
            fixedpos = true;
        }
        
        if(i % ((orig-> width)*4) == 0 && i!= 0) {
            pg+= (((gray-> width)*4) - (orig->width *4));
            
        }

        *pg = *p;
        *(pg + 1) = *(p+1);
        *(pg + 2) = *(p+2);
        
        if(orig->channels == 4) {
            *(pg + 3) = *(p + 3);
        }
        i+=orig-> channels;
    }
    return gray;
}


Image* Image_invert(Image *orig ) {
    //ON_ERROR_EXIT(!(orig->allocation_ != NO_ALLOCATION && orig->channels >= 3), "The input image must have at least 3 channels.");
    
    uint8_t pix;
    int8_t maxval = 255;
    int i = 0;
    for(unsigned char *p = orig->data; p != orig->data + orig->size; p += orig->channels) {
        pix = *p;
        *p = maxval - pix;

        pix = *(p+1);
        *(p + 1) = maxval - pix;

        pix = *(p+2);
        *(p + 2) = maxval - pix;
    }
    return orig;
}

Image* Image_add( Image *img1, Image *img2) {
    int flag, w, h;
    w = h = 1;
    flag = img1->size >= img2->size ? 1 : 2;
    int i = 0;
    Image* gray;
    if (flag == 1){
        gray = Image_create(img1->width, img1->height, pixel(0, 0, 0, 255));
        for (int g_idx = 0, img1_idx = 0, img2_idx = 0; g_idx < gray->size; g_idx+= gray->channels, img1_idx += img1->channels) {
            for (int c = 0; c < img1->channels; c++) {
                gray->data[g_idx + c] = img1->data[img1_idx + c];
            }
            if (w <= img2->width && h <= img2->height) {
                for (int c = 0; c < img2->channels; c++) {
                    gray->data[g_idx + c] += img2->data[img2_idx + c];
                }
                img2_idx += img2->channels;
            }
            if (w == gray->width) {
                w = 1;
                h += 1;
            } else {
                w += 1;
            }
        }  
    } else {
        gray = Image_create(img2->width, img2->height, pixel(0, 0, 0, 255));
        for (int g_idx = 0, img1_idx = 0, img2_idx = 0; g_idx < gray->size; g_idx+= gray->channels, img2_idx += img2->channels) {
            //printf("%d", g_idx);
            for (int c = 0; c < img2->channels; c++) {
                gray->data[g_idx + c] = img2->data[img2_idx + c];
            }
            if (w <= img1->width && h <= img1->height) {
                for (int c = 0; c < img1->channels; c++) {
                    gray->data[g_idx + c] += img1->data[img1_idx + c];
                }
                img1_idx += img1->channels;
            }
            if (w == gray->width) {
                w = 1;
                h += 1;
            } else {
                w += 1;
            }
        }  
    }
    return gray;
}

Image* Image_subtract( Image *img1, Image *img2) {
    int flag, w, h;
    w = h = 1;
    flag = img1->size >= img2->size ? 1 : 2;
    int i = 0;
    Image* gray;
    if (flag == 1){
        gray = Image_create(img1->width, img1->height, pixel(0, 0, 0, 255));
        for (int g_idx = 0, img1_idx = 0, img2_idx = 0; g_idx < gray->size; g_idx+= gray->channels, img1_idx += img1->channels) {
            for (int c = 0; c < img1->channels; c++) {
                gray->data[g_idx + c] = img1->data[img1_idx + c];
            }
            if (w <= img2->width && h <= img2->height) {
                for (int c = 0; c < img2->channels; c++) {
                    gray->data[g_idx + c] -= img2->data[img2_idx + c];
                }
                img2_idx += img2->channels;
            }
            if (w == gray->width) {
                w = 1;
                h += 1;
            } else {
                w += 1;
            }
        }  
    } else {
        gray = Image_create(img2->width, img2->height, pixel(0, 0, 0, 255));
        for (int g_idx = 0, img1_idx = 0, img2_idx = 0; g_idx < gray->size; g_idx+= gray->channels, img2_idx += img2->channels) {
            //printf("%d", g_idx);
            for (int c = 0; c < img2->channels; c++) {
                gray->data[g_idx + c] = img2->data[img2_idx + c];
            }
            if (w <= img1->width && h <= img1->height) {
                for (int c = 0; c < img1->channels; c++) {
                    gray->data[g_idx + c] -= img1->data[img1_idx + c];
                }
                img1_idx += img1->channels;
            }
            if (w == gray->width) {
                w = 1;
                h += 1;
            } else {
                w += 1;
            }
        }  
    }
    return gray;
}

Image* Image_to_gray(const Image *orig) {
    //ON_ERROR_EXIT(!(orig->allocation_ != NO_ALLOCATION && orig->channels >= 3), "The input image must have at least 3 channels.");
    Image* gray = Image_create(orig->width, orig->height, pixel(0, 0, 0, 255));
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

Image* Image_flip(const Image *orig) {
    //ON_ERROR_EXIT(!(orig->allocation_ != NO_ALLOCATION && orig->channels >= 3), "The input image must have at least 3 channels.");
    int channels = 4;
    Image* flipped = Image_create(orig->width, orig->height, pixel(0, 0, 0, 255));
    ON_ERROR_EXIT(flipped->data == NULL, "Error in creating the image");
    int index = 0;
    int flippedIndex = 0;

    for(int y = 0; y < orig->height; y++) {
        for (int x = 0; x < orig->width; x++){
            index = (x + (orig->width * y)) * channels;
            flippedIndex = ((orig->width - x) + (orig->width * y)) * channels;
            for (int c = 0; c < channels; c++) {
                flipped->data[flippedIndex + c] = orig->data[index + c];
            }
        }
    }
    return flipped;
}