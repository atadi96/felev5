#ifndef __PVM_UTILS_HPP
#define __PVM_UTILS_HPP

#include <tuple>
#include <vector>
#include <iostream>
#include <string>
#include "image.hpp"
#include "pvm3.h"

namespace pvm_utils {
    template<typename T>
    void pk(const T& image);

    template<typename T>
    T upk();

    template<>
    void pk(const Image& image) {
        Image& img = const_cast<Image&>(image);
        int size = img.size;
        pvm_pkint(&size, 1, 1);
        pvm_pkbyte((char*)img.data, image.size * image.size * sizeof(Color), 1);
    }

    template<>
    Image upk() {
        int size;
        pvm_upkint(&size, 1, 1);
        Image result(size);
        pvm_upkbyte((char*)result.data, size * size * sizeof(Color), 1);
        return result;
    }

    template<>
    void pk(const Image3bit& image) {
        Image3bit& img = const_cast<Image3bit&>(image);
        int size = img.size;
        pvm_pkint(&size, 1, 1);
        pvm_pkbyte((char*)img.data, image.size * image.size * sizeof(Color3bit), 1);
    }

    template<>
    Image3bit upk() {
        int size;
        pvm_upkint(&size, 1, 1);
        Image3bit result(size);
        pvm_upkbyte((char*)result.data, size * size * sizeof(Color3bit), 1);
        return result;
    }

    template<>
    void pk(const int& val) {
        int val2 = val;
        pvm_pkint(&val2, 1, 1);
    }

    template<>
    int upk() {
        int val;
        pvm_upkint(&val, 1, 1);
        return val;
    }

    template<>
    void pk(const std::tuple<int, Image>& data) {
        pvm_pkint( const_cast<int*>( &(std::get<0>(data)) ), 1, 1 );
        pk<Image>(std::get<1>(data));
    }
    
    template<>
    std::tuple<int, Image> upk() {
        int p;
        pvm_upkint(&p, 1, 1);
        Image img(upk<Image>());
        return std::make_tuple(p, img);
    }

    template<>
    void pk(const ColorTag& vecs) {
        int size = vecs.size();
        pvm_pkint(&size, 1, 1);
        for(const auto& vec : vecs) {
            int size2 = vec.size();
            pvm_pkint(&size2, 1, 1);
            for(int elem : vec) {
                int elemc = elem;
                pvm_pkint(&elemc, 1, 1);
            }
        }
    }

    template<>
    ColorTag upk() {
        ColorTag result;
        int size;
        pvm_upkint(&size, 1, 1);
        for(int i = 0; i < size; ++i) {
            result.push_back(std::vector<int>());
            int size2;
            pvm_upkint(&size2, 1, 1);
            for(int j = 0; j < size2; ++j) {
                int temp;
                pvm_upkint(&temp, 1, 1);
                result.at(i).push_back(temp);
            }
        }
        return result;
    }

    template<>
    void pk(const std::tuple<Image,ColorTag,ColorTag>& tup) {
        pk<Image>(std::get<0>(tup));
        pk<ColorTag>(std::get<1>(tup));
        pk<ColorTag>(std::get<2>(tup));
    }

    template<>
    std::tuple<Image,ColorTag,ColorTag> upk() {
        auto t1 = upk<Image>();
        auto t2 = upk<ColorTag>();
        auto t3 = upk<ColorTag>();
        return std::make_tuple(
                t1,
                t2,
                t3
        );
    }
    struct ChannelInfo {
        int in_pid;
        int out_pid;
    };

    template<>
    void pk(const ChannelInfo& info) {
        ChannelInfo& info2 = const_cast<ChannelInfo&>(info);
        pvm_pkint(&(info2.in_pid), 1, 1);
        pvm_pkint(&(info2.out_pid), 1, 1);
    }
    
    const ChannelInfo getChannelInfo() {
        pvm_recv(pvm_parent(), -1);
        ChannelInfo info;
        pvm_upkint(&(info.in_pid), 1, 1);
        pvm_upkint(&(info.out_pid), 1, 1);
        return info;
    }
    
    template<typename InType, typename OutType, typename Transformer>
    class ChannelChild {
    private:
        const ChannelInfo chInfo;
        Transformer trans;
        bool canRun;
        const std::string name;
    public:
        ChannelChild(Transformer tr, const std::string& name)
            : chInfo(getChannelInfo())
            , trans(tr)
            , canRun(true)
            , name(name)
            { }

        void run() {
            if(!canRun) return;
            canRun = false;
            char chEnd;
            //std::cerr << "[" << name <<"] so recv" << std::endl;
            pvm_recv(chInfo.in_pid, -1);
            pvm_upkbyte(&chEnd, 1, 1);
            while(!chEnd) {
                InType inVal(upk<InType>());
                OutType res(trans(inVal));
                pvm_initsend(PvmDataDefault);
                pvm_pkbyte(&chEnd, 1, 1);
                pk<OutType>(res);
                pvm_send(chInfo.out_pid, 0);

                //std::cerr << "[" << name <<"] so recv" << std::endl;
                pvm_recv(chInfo.in_pid, -1);
                pvm_upkbyte(&chEnd, 1, 1);
            }
            pvm_initsend(PvmDataDefault);
            pvm_pkbyte(&chEnd, 1, 1);
            pvm_send(chInfo.out_pid, 0);
            pvm_exit();
        }
    };
}

#endif