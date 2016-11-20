define("bundles/quiz-question-types/models/CheckboxQuestion",["require","exports","module","./AbstractCheckboxQuestion"],function(require,exports,module){"use strict";function _defaults(e,o){for(var i=Object.getOwnPropertyNames(o),t=0;t<i.length;t++){var n=i[t],r=Object.getOwnPropertyDescriptor(o,n);r&&r.configurable&&void 0===e[n]&&Object.defineProperty(e,n,r)}return e}function _classCallCheck(e,t){if(!(e instanceof t))throw new TypeError("Cannot call a class as a function")}function _possibleConstructorReturn(t,e){if(!t)throw new ReferenceError("this hasn't been initialised - super() hasn't been called");return!e||"object"!=typeof e&&"function"!=typeof e?t:e}function _inherits(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function, not "+typeof e);t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,enumerable:!1,writable:!0,configurable:!0}}),e&&(Object.setPrototypeOf?Object.setPrototypeOf(t,e):_defaults(t,e))}var e=require("./AbstractCheckboxQuestion"),t=function(e){function CheckboxQuestionGradedFeedback(t){return _classCallCheck(this,CheckboxQuestionGradedFeedback),_possibleConstructorReturn(this,e.call(this,t))}return _inherits(CheckboxQuestionGradedFeedback,e),CheckboxQuestionGradedFeedback}(e.GradedFeedback),n=function(e){function CheckboxQuestionExplainedFeedback(t){return _classCallCheck(this,CheckboxQuestionExplainedFeedback),_possibleConstructorReturn(this,e.call(this,t))}return _inherits(CheckboxQuestionExplainedFeedback,e),CheckboxQuestionExplainedFeedback}(e.ExplainedFeedback),r=function(e){function CheckboxQuestionResponse(t){return _classCallCheck(this,CheckboxQuestionResponse),_possibleConstructorReturn(this,e.call(this,t))}return _inherits(CheckboxQuestionResponse,e),CheckboxQuestionResponse}(e.Response),o=function(e){function CheckboxQuestion(t){return _classCallCheck(this,CheckboxQuestion),_possibleConstructorReturn(this,e.call(this,t))}return _inherits(CheckboxQuestion,e),CheckboxQuestion.prototype.createGradedFeedback=function createGradedFeedback(e){return new t(e)},CheckboxQuestion.prototype.createExplainedFeedback=function createExplainedFeedback(e){return new n(e)},CheckboxQuestion.prototype.createResponse=function createResponse(e){return new r(e)},CheckboxQuestion}(e);module.exports=o,module.exports.GradedFeedback=t,module.exports.ExplainedFeedback=n,module.exports.Response=r});