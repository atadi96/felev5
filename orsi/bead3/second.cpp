#include <tuple>
#include "image.hpp"
#include "pvm_utils.hpp"
#include "resize.hpp"

// Anta, baka?!

std::vector<Color3bit> convert_row(const int row, const Image& image) {
    std::vector<Color3bit> result;
    for(int x = 0; x < image.size; ++x) {
        result.push_back(image.get(x, row).to3bit());
    }
    return result;
}

int main() {
    auto f2 = [](const Image& image) -> Image3bit {
        std::vector<std::future<std::vector<Color3bit>>> futures;
        for(int row = 1; row < image.size; ++row) {
            futures.push_back(std::async(std::launch::async, convert_row, row, image));
        }
        std::vector<Color3bit> temp = convert_row(0, image);
        Image3bit result(image.size);
        for(int x = 0; x < image.size; ++x) {
            result.get(x, 0) = temp[x];
        }
        for(int row = 1; row < image.size; ++row) {
            std::vector<Color3bit> temp = futures[row-1].get();
            for(int x = 0; x < image.size; ++x) {
                result.get(x, row) = temp[x];
            }
        }
        return result;
    };
    auto channel =
        pvm_utils::ChannelChild
            < Image
            , Image3bit
            , decltype(f2)
            >( f2, "second" );
    channel.run();
    return 0;
}
