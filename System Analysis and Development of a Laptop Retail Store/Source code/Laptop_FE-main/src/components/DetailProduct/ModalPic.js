import React, { useEffect, useState } from 'react';
import PropTypes from 'prop-types';


const ModalPic = (props) => {
    const {src,QuitModal,ChangeModal}=props;
    useEffect(() => {
        var modal_body = document.querySelector(".modal__body");
        window.addEventListener("click",(e)=>{
            if(e.target==modal_body){
                QuitModal();
            }
        });
    }, []);
    return (
        <div className="modal1" style={{display:"block"}}>
            <div className="modal__body">
                <button class="modal--button modal--button-quit" onClick={QuitModal}><i class="fas fa-times"></i></button>
                <button class="modal--button modal--button-left" onClick={()=>ChangeModal(-1)}><i class="fas fa-chevron-left"></i></button>
                <div className="modal__body__sign-in modal__body__page modal__body--image">
                    <div className="modal__body__form__wrap">
                        <img src={src.item} class="modal__body--image-detail"></img>
                    </div>
                </div>
                <button class="modal--button modal--button-right" onClick={()=>ChangeModal(1)}><i class="fas fa-chevron-right"></i></button>
            </div>
        </div>
    );
};


ModalPic.propTypes = {

};


export default ModalPic;
