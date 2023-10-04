import * as types from '../constants/ActionType';
const initialState = {
    isOpenForm: false,
    data: null,
};
const reducer = (state = initialState, action) => {
    switch (action.type) {
        case types.UPDATE_PRODUCT:
            return {
                isOpenForm:true,
                data: action.data,
            }
        case types.SHOW_FORM_PRODUCT:
            return {
                data: null,
                isOpenForm:true,
            }
        case types.CLOSE_FORM_PRODUCT:
            return {
                data: null,
                isOpenForm:false,
            }
        default:
            return state;
    }
}
export default reducer;