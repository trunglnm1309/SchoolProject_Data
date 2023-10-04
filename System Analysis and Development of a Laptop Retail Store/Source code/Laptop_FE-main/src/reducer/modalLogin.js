import * as types from '../constants/ActionType';
// 0 không hiển thị 1 hiển thị login 2 hiển thị register
const initialState = 0;

const reducer = (state = initialState, action) => {
    switch (action.type) {
        case types.SHOW_MODAL_LOGIN:
            return 1;
        case types.SHOW_MODAL_REGISTER:
            return 2;
        case types.SHOW_FORGET_PASSWORD:
            return 3;
        case types.CLOSE_MODAL_LOGIN:
            return 0;
        default:
            return state;
    }
}
export default reducer;