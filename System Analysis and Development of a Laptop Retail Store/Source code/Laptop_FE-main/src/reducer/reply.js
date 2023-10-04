import * as types from '../constants/ActionType';
const initialState = null;
const reducer = (state = initialState, action) => {
    switch (action.type) {
        case types.GET_KEY_REPLY:
            return action.key;
        default:
            return state;
    }
}
export default reducer;