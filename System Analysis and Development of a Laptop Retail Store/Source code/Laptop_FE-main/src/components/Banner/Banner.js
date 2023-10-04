import React from 'react';
import PropTypes from 'prop-types';

const BannerItem = (props) => {
    const {source}=props;
   return( <div class="col">
        <img src={source} alt="" class="banner-img" />
    </div>)
}
const Banner = (props) => {
    const {listBanner}=props;
    function renderBannerItem(){
        var result=[];
        if(listBanner){
            result=listBanner.map((item,index)=><BannerItem key={index} source={item}></BannerItem>)
        }
        return result;
    }
    return (
        <div class="banner">
            <div class="container">
                <div class="row">
                    {renderBannerItem()}
                </div>
            </div>
        </div>
    );
};


Banner.propTypes = {

};


export default Banner;
