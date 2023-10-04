import * as types from './../constants/ActionType';

export const addOrder = (data)=>{
    return {
        type : types.ADD_ODER,
        data : data,
    }
}
export const openOrderDetail = ()=>{
    return {type: types.SHOW_ORDER_DETAIL}
}
export const closeOrderDetail = ()=>{
    return {type: types.CLOSE_ORDER_DETAIL}
}