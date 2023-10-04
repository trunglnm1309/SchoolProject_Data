import * as types from '../constants/ActionType';
const initialState = null;
const reducer = (state = initialState, action) => {
    switch (action.type) {
        case types.ADD_ODER:
            return {...action.data}
        default:
            return state;
    }
}
export default reducer;