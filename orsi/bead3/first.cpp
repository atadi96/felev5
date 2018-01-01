#include <tuple>
#include <iostream>
#include "image.hpp"
#include "pvm_utils.hpp"
#include "resize.hpp"

// If I die, I can be replaced.

int main() {
    auto f1 = [](const std::tuple<int,Image>& data) -> Image {
        int p = std::get<0>(data);
        const Image& img = std::get<1>(data);
        slicing::ImagePart initial(0, 0, img.size/p, img.size/p, p, img);
        return resize::resize(initial).toImage();
    };
    auto channel =
        pvm_utils::ChannelChild
            < std::tuple<int, Image>
            , Image
            , decltype(f1)
            >( f1, "first");
    channel.run();
    return 0;
}
