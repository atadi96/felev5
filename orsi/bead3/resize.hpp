#include "image.hpp"
#include <algorithm>
#include <vector>
#include <future>

namespace resize {

    const int CRIT_SIZE = 3;

    Color average(int x, int y, int p, const Image& img) {
        unsigned int r = 0;
        unsigned int g = 0;
        unsigned int b = 0;
        int count = 0;
        //std::cout << x << " " << y << std::endl;
        for(int k = 0; k < p; ++k) {
            for(int l = 0; l < p; ++l) {
                if(img.isSafeCoord(x + k, y + l)) {
                    //std::cout << "   " << x + k << " " << y + l << std::endl;
                    const Color& color = img.get(x + k, y + l);
                    r += color.red;
                    g += color.green;
                    b += color.blue;
                    ++count;
                }
            }
        }
        if(count == 0) {
            return Color(0,0,0);
        } else {
            r /= count;
            g /= count;
            b /= count;
            return Color(r,g,b);
        }
    }

    using namespace slicing;

    SlicingData resize(const ImagePart& imgPart) {
        if(imgPart.sx <= CRIT_SIZE || imgPart.sy <= CRIT_SIZE) {
            SlicingData result;
            result.sx = imgPart.sx;
            result.sy = imgPart.sy;
            ColorData& data = result.colorData;
            for(int y = 0; y < imgPart.sy; ++y) {
                std::vector<Color> row;
                data.push_back(row);
                for(int x = 0; x < imgPart.sx; ++x) {
                    data[y].push_back(
                        average(
                            (imgPart.x + x) * imgPart.p,
                            (imgPart.y + y) * imgPart.p,
                            imgPart.p,
                            imgPart.image
                        )
                    );
                }
            }
            return result;
        } else {
            std::vector<ImagePart> subtasks(slice(imgPart));
            std::vector<std::future<SlicingData>> subresults;
            std::transform(
                subtasks.begin(),
                subtasks.end(),
                std::back_inserter(subresults),
                [](const ImagePart& imgPart) {
                    return std::async(std::launch::async, resize, imgPart);
                }
            );
            std::vector<SlicingData> results;
            std::transform(
                subresults.begin(),
                subresults.end(),
                std::back_inserter(results),
                [](std::future<SlicingData>& proc) {
                    return proc.get();
                }
            );
            return combine(results);
        }
    }

}
