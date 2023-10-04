import React from 'react';
import PropTypes from 'prop-types';


const Slideitem = (props) => {
    const {urlImage} = props;
    return (
        <div class="item">
            <img src={urlImage} alt="" class="header-slide-img" />
        </div>
    );
};


Slideitem.propTypes = {

};


export default Slideitem;
