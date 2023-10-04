import * as types from './../constants/ActionType';

export const UpdateProduct = (data)=>{
    return {
        type : types.UPDATE_PRODUCT,
        data : data,
    }
}
export const OpenFormProduct = ()=>{
    return {
        type: types.SHOW_FORM_PRODUCT
    }
}
export const CloseFormProduct = ()=>{
    return {
        type: types.CLOSE_FORM_PRODUCT
    }
}