import * as types from '../constants/ActionType';
// true show detail order false close order
const initialState = false;

const reducer = (state = initialState, action) => {
    switch (action.type) {
        case types.SHOW_ORDER_DETAIL:
            return true;
        case types.CLOSE_ORDER_DETAIL:
            return false;
        
        default:
            return state;
    }
}
export default reducer;