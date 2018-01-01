#ifndef __IMAGE_HPP
#define __IMAGE_HPP

#include <iostream>
#include <vector>
/*
*-x->
|
y
|
*/

using ColorTag = std::vector<std::vector<int>>;

enum class Color3bit : unsigned char {
    BLACK = 0b000,
    RED = 0b100,
    GREEN = 0b010,
    BLUE = 0b001,
    CYAN = 0b011,
    MAGENTA = 0b101,
    YELLOW = 0b110,
    WHITE = 0b111
};

struct Color {
    typedef unsigned int member_type;
    member_type red;
    member_type green;
    member_type blue;

    Color() {}

    Color(unsigned int r, unsigned int g, unsigned int b)
        : red(r), green(g), blue(b) {}

    Color(const Color& color)
        : red(color.red)
        , green(color.green)
        , blue(color.blue) {}

    Color(const Color3bit& c3b) {
        unsigned char repr = static_cast<unsigned char>(c3b);
        red = (repr & 0b100) > 0 ? 255 : 0;
        green = (repr & 0b010) > 0 ? 255 : 0;
        blue = (repr & 0b001) > 0 ? 255 : 0;
    }

    Color& operator+=(Color color) {
        red = (red + color.red) % 256;
        green = (green + color.green) % 256;
        blue = (blue + color.blue) % 256;
        return *this;
    }
    
    Color operator+(Color color) {
        Color c(*this);
        c += color;
        return c;
    }

    Color& operator/=(unsigned int p) {
        red /= p;
        green /= p;
        blue /= p;
        return *this;
    }
    
    Color3bit to3bit() const {
        unsigned char r = (red > 127) ? 4 : 0;
        unsigned char g = (green > 127) ? 2 : 0;
        unsigned char b = (blue > 127) ? 1 : 0;
        return static_cast<Color3bit>(r + g + b);
    }
};

std::istream& operator>>(std::istream& is, Color& color) {
    is >> color.red >> color.green >> color.blue;
    return is;
}

std::ostream& operator<<(std::ostream& os, const Color c) {
    os << "(" << c.red << "," << c.green << "," << c.blue << ") ";
    return os;
}

template<typename ImgType, typename ColorType>
struct image_row_iterator {
private:
    int x;
    int y;
    const ImgType* const image;
    image_row_iterator(int size) : x(size), image(nullptr) {}
public:
    using elem_type = ColorType;
    image_row_iterator(int row, const ImgType& img)
        : x(0)
        , y(row)
        , image(&img)
        { }
    image_row_iterator<ImgType,ColorType>& operator++() {
        ++x;
        return *this;
    }
    const ColorType& operator*() {
        return image->get(x, y);
    }
    bool operator==(const image_row_iterator<ImgType,ColorType>& other) {
        return x == other.x;
    }
    bool operator!=(const image_row_iterator<ImgType,ColorType>& other) {
        return x != other.x;
    }
    static image_row_iterator<ImgType,ColorType> end(const ImgType& img) {
        return image_row_iterator<ImgType,ColorType>(img.size);
    }
};

template<typename ImgType, typename ColorType>
struct image_col_iterator {
private:
    int x;
    int y;
    const ImgType* const image;
    image_col_iterator(int size) : y(size), image(nullptr) {}
public:
    using elem_type = ColorType;
    image_col_iterator(int col, const ImgType& img)
        : x(col)
        , y(0)
        , image(&img)
        { }
    image_col_iterator<ImgType,ColorType>& operator++() {
        ++y;
        return *this;
    }
    const ColorType& operator*() {
        return image->get(x, y);
    }
    bool operator==(const image_col_iterator<ImgType,ColorType>& other) {
        return y == other.y;
    }
    bool operator!=(const image_col_iterator<ImgType,ColorType>& other) {
        return y != other.y;
    }
    static image_col_iterator<ImgType,ColorType> end(const ImgType& img) {
        return image_col_iterator<ImgType,ColorType>(img.size);
    }
};

struct Image {
public:
    const int size;
    Color* const data;

    Image(int size)
        : size(size)
        , data(new Color[size*size])
        { }

    Image(const Image& other) : Image(other.size) {
        for(int i = 0; i < size*size; ++i) {
            data[i] = other.data[i];
        }
    }

    ~Image() {
        delete[] data;
    }

    bool isSafeCoord(int x, int y) const {
        return 0 <= x
            && 0 <= y
            && x < size
            && y < size;
    }

    Color& get(int x, int y) {
        return data[x + size * y];
    }

    const Color& get(int x, int y) const {
        return data[x + size * y];
    }

    static Image fromStream(std::istream& is) {
        int size;
        is >> size;
        Image image(size);
        for(int i = 0; i < size * size; ++i) {
            is >> image.data[i];
        }
        return image;
    }
private:
    Image& operator=(const Image&);
};

std::ostream& operator<<(std::ostream& os, const Image& img) {
    os << img.size << std::endl;
    for(int y = 0; y < img.size; ++y) {
        for(int x = 0; x < img.size; ++x) {
            os << img.get(x,y);
        }
        os << std::endl;
    }
    return os;
}

struct Image3bit {
    const int size;
    Color3bit* const data;

    using row_iterator = image_row_iterator<Image3bit,Color3bit>;
    using col_iterator = image_col_iterator<Image3bit,Color3bit>;

    Image3bit(int size)
        : size(size)
        , data(new Color3bit[size*size])
        { }
    
    Image3bit(const Image3bit& other) : Image3bit(other.size) {
        for(int i = 0; i < size*size; ++i) {
            data[i] = other.data[i];
        }
    }
    
    ~Image3bit() {
        delete[] data;
    }
    
    bool isSafeCoord(int x, int y) const {
        return 0 <= x
            && 0 <= y
            && x < size
            && y < size;
        }
        
    Image toImage() const {
        Image img(size);
        for(int x = 0; x < size; ++x) {
            for(int y = 0; y < size; ++y) {
                img.get(x,y) = Color(get(x,y));
            }
        }
        return img;
    }
    
    Color3bit& get(int x, int y) {
        return data[x + size * y];
    }

    const Color3bit& get(int x, int y) const {
        return data[x + size * y];
    }

    row_iterator begin_row(int row) const {
        return row_iterator(row, *this);
    }

    row_iterator end_row() const {
        return row_iterator::end(*this);
    }

    col_iterator begin_col(int col) const {
        return col_iterator(col, *this);
    }

    col_iterator end_col() const {
        return col_iterator::end(*this);
    }
};

namespace slicing {
    typedef std::vector<std::vector<Color>> ColorData;

    struct ImagePart {
        int x;
        int y;
        int sx;
        int sy;
        int p;
        const Image& image;

        ImagePart(const Image& image) : image(image) { };

        ImagePart(int x, int y, int sx, int sy, int p, const Image& image)
            : x(x)
            , y(y)
            , sx(sx)
            , sy(sy)
            , p(p)
            , image(image)
            { }
    };

    struct SlicingData {
        int sx;
        int sy;
        ColorData colorData;

        Image toImage() const {
            Image result(sx);
            int index = 0;
            for(auto& row : colorData) {
                for(auto& color : row) {
                    result.data[index] = color;
                    ++index;
                }
            }
            return result;
        }
    };

    SlicingData combine(std::vector<SlicingData>& data) {
        SlicingData result;
        int sx = data[0].sx + data[1].sx;
        int sy = data[0].sy + data[2].sy;
        int x, y;
        for(y = 0; y < data[0].sy; ++y) {
            std::vector<Color> row;
            result.colorData.push_back(row);
            for(x = 0; x < data[0].sx; ++x) {
                result.colorData[y].push_back(data[0].colorData.at(y).at(x));
            }
            for(; x < sx; ++x) {
                result.colorData[y].push_back(data[1].colorData.at(y).at(x-data[0].sx));
            }
        }
        for(; y < sy; ++y) {
            std::vector<Color> row;
            result.colorData.push_back(row);
            for(x = 0; x < data[0].sx; ++x) {
                result.colorData[y].push_back(data[2].colorData.at(y-data[0].sy).at(x));
            }
            for(; x < sx; ++x) {
                result.colorData[y].push_back(data[3].colorData.at(y-data[0].sy).at(x-data[0].sx));
            }
        }
        result.sx = sx;
        result.sy = sy;
        return result;
    }
    
    std::vector<ImagePart> slice(const ImagePart& imgPart) {
        int sx1 = imgPart.sx / 2;
        int sy1 = imgPart.sy / 2;
        int sx2 = imgPart.sx - sx1;
        int sy2 = imgPart.sy - sy1;
        const ImagePart& ip = imgPart;
        std::vector<ImagePart> result;
        result.push_back(ImagePart(ip.x,             ip.y, sx1, sy1, imgPart.p, imgPart.image));
        result.push_back(ImagePart(ip.x + sx1,       ip.y, sx2, sy1, imgPart.p, imgPart.image));
        result.push_back(ImagePart(ip.x,       ip.y + sy1, sx1, sx2, imgPart.p, imgPart.image));
        result.push_back(ImagePart(ip.x + sx1, ip.y + sy1, sx2, sy2, imgPart.p, imgPart.image));
        return result;
    }
}

#endif