import * as types from './../constants/ActionType';

export const getKey = (key)=>{
    return {
        type : types.GET_KEY_REPLY,
        key,
    }
}