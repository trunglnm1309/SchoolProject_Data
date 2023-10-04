import React, { useEffect } from 'react';
import Slider from "react-slick";
import Slideitem from './slideitem';
import './slider.css';
const NextSlide=(props)=>{
    const {onClick}=props;
    return <span class="header-slide-btn header-slide-left" onClick={onClick}><i class="fas fa-chevron-left"></i></span>
}
const PrevSlide=(props)=>{
    const {onClick}=props;
    return <span class="header-slide-btn header-slide-right" onClick={onClick}><i class="fas fa-chevron-right"></i></span>
}
const Slide = (props) => {
    const {setting}=props;
    var listImage=["https://lh3.googleusercontent.com/co6o531pxSg_zL589TTQHReI07caHOgsCQ4RiO0GyGIQLjvc3HjOvN0n3JquqLHUPrcYI-JRc606pqm75-LV4KuU4aDhtGml=w1920-rw","https://lh3.googleusercontent.com/ExtEtxrtXOlDBtPViPYNV5KaYuYNQx_w6MscUXEzUwydqf_JSwZV7iuL_EMTiqfRvR6RULbeqQ4yns2AoZJnQba4AkzaKAYv=w1920-rw"];
    function renderSlide(){
        var result=[];
        for(var i=0;i<listImage.length;i++){
            result.push(<Slideitem urlImage={listImage[i]} key={i}>
            </Slideitem>)
        }
        return result;
    }
    var settings = {
        infinite: true,
        speed: 500,
       ...setting,
       slidesToShow: 1,
       slidesToScroll: 1,
       nextArrow:<PrevSlide/>,
       prevArrow:<NextSlide/>,
    };
    return (
        <Slider {...settings}>
            {renderSlide()}
        </Slider>
    );
};


Slide.propTypes = {

};


export default Slide;
