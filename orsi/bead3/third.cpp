#include <future>
#include <tuple>
#include "image.hpp"
#include "pvm_utils.hpp"
#include "resize.hpp"

// I mustn't run away!!!

template<typename ImgIterator>
std::vector<int> getLineTag(ImgIterator it, ImgIterator end) {
    std::vector<int> result;
    typename ImgIterator::elem_type current;
    while(it != end) {
        current = *it;
        int current_num = 0;
        while(it != end && current == *it) {
            ++it;
            ++current_num;
        }
        result.push_back(current_num);
    }
    return result;
}

int main() {
    auto f3 = [](const Image3bit& data) -> std::tuple<Image,ColorTag,ColorTag> {
        std::vector<std::future<std::vector<int>>> f_rows;
        std::vector<std::future<std::vector<int>>> f_cols;
        for(int i = 0; i < data.size; ++i) {
            f_rows.push_back(std::async(std::launch::async, getLineTag<Image3bit::row_iterator>, data.begin_row(i), data.end_row()));
            f_cols.push_back(std::async(std::launch::async, getLineTag<Image3bit::col_iterator>, data.begin_col(i), data.end_col()));
        }
        Image img = data.toImage();
        ColorTag rows;
        ColorTag cols;
        for(int i = 0; i < data.size; ++i) {
            rows.push_back(f_rows[i].get());
            cols.push_back(f_cols[i].get());
        }
        return std::make_tuple(img,rows,cols);
    };
    auto channel =
        pvm_utils::ChannelChild
            < Image3bit
            , std::tuple<Image,ColorTag,ColorTag>
            , decltype(f3)
            >( f3, "third" );
    channel.run();
    return 0;
}
