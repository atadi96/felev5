#include <iostream>
#include <chrono>
#include <cstdlib>
#include <fstream>
#include <string>
#include "image.hpp"
#include "pvm_utils.hpp"
#include "pvm3.h"

// You did well, Shinji.

std::ostream& operator<<(std::ostream& os, const std::tuple<Image,ColorTag,ColorTag>& result) {
    os << std::get<0>(result);
    const ColorTag& rows = std::get<1>(result);
    for(auto& rowtags : rows) {
        for(auto& num : rowtags) {
            os << num << " ";
        }
        os << std::endl;
    }
    const ColorTag& cols = std::get<2>(result);
    for(auto& coltags: cols) {
        for(auto& num : coltags) {
            os << num << " ";
        }
        os << std::endl;
    }
    return os;
}

int main(int argc, char** argv) {
    if(argc!=4) {
        std::cerr << "Usage: master scale infile outfile" << std::endl;
        return 1;
    } else {
        int scale = atoi(argv[1]);
        std::fstream in_file(argv[2]);

        int length;
        std::vector<Image> images;

        in_file >> length;
        for(int i = 0; i < length; ++i) {
            images.push_back(Image::fromStream(in_file));
        }

        auto start = std::chrono::system_clock::now();

        //init children and channel
        int mytid = pvm_mytid();
        
        int first_id;
        pvm_spawn(const_cast<char*>("first"), (char**)0, 0, const_cast<char*>(""), 1, &first_id);
        int second_id;
        pvm_spawn(const_cast<char*>("second"), (char**)0, 0, const_cast<char*>(""), 1, &second_id);
        int third_id;
        pvm_spawn(const_cast<char*>("third"), (char**)0, 0, const_cast<char*>(""), 1, &third_id);

        pvm_utils::ChannelInfo firstChannel;
        firstChannel.in_pid = mytid;
        firstChannel.out_pid = second_id;
        
        pvm_utils::ChannelInfo secondChannel;
        secondChannel.in_pid = first_id;
        secondChannel.out_pid = third_id;
        
        pvm_utils::ChannelInfo thirdChannel;
        thirdChannel.in_pid = second_id;
        thirdChannel.out_pid = mytid;

        pvm_initsend(PvmDataDefault);
        pvm_utils::pk(firstChannel);
        pvm_send(first_id, 0);

        pvm_initsend(PvmDataDefault);
        pvm_utils::pk(secondChannel);
        pvm_send(second_id, 0);

        pvm_initsend(PvmDataDefault);
        pvm_utils::pk(thirdChannel);
        pvm_send(third_id, 0);
        
        //start the channel
        char chEnd = 0;
        int p = 100 / scale;
        for(Image& img : images) {
            pvm_initsend(PvmDataDefault);
            pvm_pkbyte(&chEnd, 1, 1);
            pvm_utils::pk<std::tuple<int,Image>>(std::make_tuple(p, img));
            pvm_send(first_id, 0);
        }
        chEnd = 1;
        pvm_initsend(PvmDataDefault);
        pvm_pkbyte(&chEnd, 1, 1);
        pvm_send(first_id, 0);
        //recieve results
        
        //pvm_recv(first_id, -1);
        std::vector<std::tuple<Image,ColorTag,ColorTag>> results;
        pvm_recv(third_id, -1);
        pvm_upkbyte(&chEnd, 1, 1);
        while(!chEnd) {
            results.push_back(pvm_utils::upk< std::tuple<Image,ColorTag,ColorTag> >());
            
            pvm_recv(third_id, -1);
            pvm_upkbyte(&chEnd, 1, 1);
        }
        
        pvm_exit();

        auto end = std::chrono::system_clock::now();

        std::chrono::duration<double> elapsed = end - start;
        std::cerr << "Elapsed: " << elapsed.count() << std::endl;

        std::ofstream out_file(argv[3]);
        for(auto& result : results) {
            out_file << result;
        }

        return 0;
    }
}
