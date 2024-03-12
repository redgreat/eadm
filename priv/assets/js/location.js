/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-03-07 11:29:20
 *
 * Module : location.js
 *
 */

let carSpeed = 5000;//移动速度，后面调整速度已这个为基准
// let carMarker; //回放车辆
let VEHICLE_PLAY_PROCESS = 0; //车辆当前回放到的百分比进度
let VEHICLE_PATH_REPLAY_START = 0;//当前回放的起点索引, 改变速度和改变进度条的时候，根据进度条百分比重新计算该数值，并从该位置开始再次回放

// 车辆轨迹、速度数据
let routeInfo = [{"lng":123.390381,"lat":41.929537,"speed":0},{"lng":123.390315,"lat":41.929736,"speed":13},{"lng":123.390331,"lat":41.930286,"speed":19},{"lng":123.390365,"lat":41.930819,"speed":25},{"lng":123.390381,"lat":41.931436,"speed":25},{"lng":123.390398,"lat":41.932069,"speed":24},{"lng":123.390416,"lat":41.932686,"speed":24},{"lng":123.390482,"lat":41.933203,"speed":20},{"lng":123.390582,"lat":41.933269,"speed":19},{"lng":123.390716,"lat":41.933287,"speed":19},{"lng":123.391433,"lat":41.933337,"speed":24},{"lng":123.392268,"lat":41.933305,"speed":26},{"lng":123.392402,"lat":41.933238,"speed":26},{"lng":123.392652,"lat":41.933071,"speed":24},{"lng":123.392819,"lat":41.932788,"speed":26},{"lng":123.393085,"lat":41.932205,"speed":30},{"lng":123.393536,"lat":41.931422,"speed":35},{"lng":123.393803,"lat":41.930956,"speed":39},{"lng":123.39407,"lat":41.930472,"speed":39},{"lng":123.394287,"lat":41.930006,"speed":41},{"lng":123.394504,"lat":41.929506,"speed":43},{"lng":123.394704,"lat":41.928972,"speed":44},{"lng":123.394921,"lat":41.928423,"speed":46},{"lng":123.395137,"lat":41.927857,"speed":46},{"lng":123.395354,"lat":41.92729,"speed":48},{"lng":123.395588,"lat":41.926707,"speed":46},{"lng":123.395822,"lat":41.926123,"speed":49},{"lng":123.396055,"lat":41.92554,"speed":47},{"lng":123.396338,"lat":41.924991,"speed":44},{"lng":123.396639,"lat":41.924508,"speed":41},{"lng":123.396939,"lat":41.924091,"speed":35},{"lng":123.397506,"lat":41.923391,"speed":28},{"lng":123.397974,"lat":41.922825,"speed":22},{"lng":123.398407,"lat":41.922375,"speed":13},{"lng":123.398841,"lat":41.921942,"speed":15},{"lng":123.399241,"lat":41.921525,"speed":24},{"lng":123.399791,"lat":41.920975,"speed":31},{"lng":123.400159,"lat":41.92061,"speed":35},{"lng":123.400876,"lat":41.919876,"speed":37},{"lng":123.401277,"lat":41.91946,"speed":41},{"lng":123.401693,"lat":41.919026,"speed":41},{"lng":123.40211,"lat":41.918576,"speed":43},{"lng":123.402527,"lat":41.918127,"speed":41},{"lng":123.402911,"lat":41.917727,"speed":37},{"lng":123.403361,"lat":41.917077,"speed":30},{"lng":123.403378,"lat":41.916877,"speed":23},{"lng":123.403261,"lat":41.916811,"speed":23},{"lng":123.402994,"lat":41.916761,"speed":26},{"lng":123.402043,"lat":41.916676,"speed":37},{"lng":123.401342,"lat":41.916643,"speed":41},{"lng":123.400659,"lat":41.91661,"speed":37},{"lng":123.400041,"lat":41.916593,"speed":33},{"lng":123.39899,"lat":41.916542,"speed":28},{"lng":123.398123,"lat":41.916509,"speed":22},{"lng":123.397839,"lat":41.916475,"speed":14},{"lng":123.397806,"lat":41.916325,"speed":15},{"lng":123.397939,"lat":41.915509,"speed":26},{"lng":123.397973,"lat":41.914675,"speed":31},{"lng":123.398073,"lat":41.913825,"speed":33},{"lng":123.398123,"lat":41.913375,"speed":35},{"lng":123.398173,"lat":41.912892,"speed":39},{"lng":123.398206,"lat":41.912425,"speed":34},{"lng":123.398289,"lat":41.911592,"speed":38},{"lng":123.398356,"lat":41.911075,"speed":43},{"lng":123.398389,"lat":41.910526,"speed":41},{"lng":123.398439,"lat":41.91001,"speed":37},{"lng":123.398488,"lat":41.909543,"speed":35},{"lng":123.398538,"lat":41.90876,"speed":28},{"lng":123.398555,"lat":41.90806,"speed":31},{"lng":123.398555,"lat":41.907143,"speed":41},{"lng":123.398572,"lat":41.90666,"speed":39},{"lng":123.398555,"lat":41.905793,"speed":31},{"lng":123.398555,"lat":41.905226,"speed":13},{"lng":123.398555,"lat":41.904743,"speed":22},{"lng":123.398572,"lat":41.904093,"speed":30},{"lng":123.398572,"lat":41.90326,"speed":33},{"lng":123.398588,"lat":41.90251,"speed":26},{"lng":123.398554,"lat":41.901893,"speed":22},{"lng":123.398504,"lat":41.90171,"speed":16},{"lng":123.398421,"lat":41.90166,"speed":16},{"lng":123.398121,"lat":41.90171,"speed":15},{"lng":123.397403,"lat":41.901793,"speed":26},{"lng":123.396285,"lat":41.901875,"speed":35},{"lng":123.395668,"lat":41.901892,"speed":37},{"lng":123.394983,"lat":41.901941,"speed":41},{"lng":123.394232,"lat":41.901991,"speed":44},{"lng":123.393516,"lat":41.902023,"speed":41},{"lng":123.392831,"lat":41.902073,"speed":39},{"lng":123.392197,"lat":41.902072,"speed":35},{"lng":123.391062,"lat":41.902072,"speed":31},{"lng":123.390094,"lat":41.902071,"speed":26},{"lng":123.38941,"lat":41.902087,"speed":17},{"lng":123.388675,"lat":41.90207,"speed":26},{"lng":123.387857,"lat":41.902069,"speed":20},{"lng":123.387189,"lat":41.902036,"speed":17},{"lng":123.386471,"lat":41.902002,"speed":18},{"lng":123.386371,"lat":41.901935,"speed":22},{"lng":123.386338,"lat":41.901818,"speed":20},{"lng":123.386371,"lat":41.901652,"speed":20},{"lng":123.386671,"lat":41.901185,"speed":0},{"lng":123.386939,"lat":41.900785,"speed":22},{"lng":123.387389,"lat":41.900169,"speed":33},{"lng":123.387657,"lat":41.899736,"speed":39},{"lng":123.38794,"lat":41.899286,"speed":39},{"lng":123.388257,"lat":41.898787,"speed":44},{"lng":123.388591,"lat":41.898254,"speed":44},{"lng":123.388924,"lat":41.897754,"speed":43},{"lng":123.389209,"lat":41.897287,"speed":39},{"lng":123.389442,"lat":41.896855,"speed":37},{"lng":123.38991,"lat":41.896055,"speed":32},{"lng":123.390193,"lat":41.895605,"speed":7},{"lng":123.390376,"lat":41.895355,"speed":13},{"lng":123.390694,"lat":41.894889,"speed":24},{"lng":123.391161,"lat":41.894172,"speed":31},{"lng":123.391595,"lat":41.893489,"speed":26},{"lng":123.391978,"lat":41.892957,"speed":15},{"lng":123.392346,"lat":41.89254,"speed":26},{"lng":123.392863,"lat":41.891907,"speed":37},{"lng":123.393214,"lat":41.891458,"speed":41},{"lng":123.393597,"lat":41.890958,"speed":50},{"lng":123.394081,"lat":41.890358,"speed":59},{"lng":123.394648,"lat":41.889709,"speed":63},{"lng":123.395182,"lat":41.889042,"speed":59},{"lng":123.395616,"lat":41.888425,"speed":55},{"lng":123.396033,"lat":41.887909,"speed":44},{"lng":123.3964,"lat":41.887476,"speed":35},{"lng":123.396884,"lat":41.886826,"speed":28},{"lng":123.397051,"lat":41.886593,"speed":24},{"lng":123.397284,"lat":41.88651,"speed":10},{"lng":123.398119,"lat":41.886594,"speed":11},{"lng":123.39877,"lat":41.886677,"speed":26},{"lng":123.399821,"lat":41.886727,"speed":35},{"lng":123.400437,"lat":41.886744,"speed":37},{"lng":123.401105,"lat":41.886778,"speed":42},{"lng":123.401788,"lat":41.886795,"speed":41},{"lng":123.402423,"lat":41.886828,"speed":36},{"lng":123.403457,"lat":41.886878,"speed":30},{"lng":123.404507,"lat":41.886929,"speed":30},{"lng":123.404925,"lat":41.886929,"speed":21},{"lng":123.404975,"lat":41.886896,"speed":21},{"lng":123.405041,"lat":41.886813,"speed":19},{"lng":123.405108,"lat":41.886446,"speed":24},{"lng":123.405208,"lat":41.885679,"speed":35},{"lng":123.405308,"lat":41.884813,"speed":30},{"lng":123.405391,"lat":41.884079,"speed":30},{"lng":123.405491,"lat":41.883213,"speed":33},{"lng":123.405591,"lat":41.882479,"speed":17},{"lng":123.405658,"lat":41.881863,"speed":27},{"lng":123.405674,"lat":41.881096,"speed":33},{"lng":123.40569,"lat":41.880629,"speed":39},{"lng":123.405724,"lat":41.880113,"speed":37},{"lng":123.405757,"lat":41.879613,"speed":41},{"lng":123.40579,"lat":41.879046,"speed":44},{"lng":123.405824,"lat":41.878479,"speed":44},{"lng":123.405857,"lat":41.877929,"speed":43},{"lng":123.405874,"lat":41.877429,"speed":39},{"lng":123.405924,"lat":41.876746,"speed":19},{"lng":123.405957,"lat":41.876279,"speed":11},{"lng":123.406024,"lat":41.875729,"speed":24},{"lng":123.406107,"lat":41.874946,"speed":35},{"lng":123.406157,"lat":41.874463,"speed":37},{"lng":123.40619,"lat":41.874013,"speed":37},{"lng":123.40624,"lat":41.873496,"speed":43},{"lng":123.40629,"lat":41.872929,"speed":50},{"lng":123.406357,"lat":41.872329,"speed":45},{"lng":123.40639,"lat":41.871796,"speed":39},{"lng":123.406439,"lat":41.871296,"speed":37},{"lng":123.406489,"lat":41.870829,"speed":35},{"lng":123.406506,"lat":41.870046,"speed":30},{"lng":123.406573,"lat":41.869346,"speed":24},{"lng":123.406656,"lat":41.869046,"speed":24},{"lng":123.406756,"lat":41.868979,"speed":20},{"lng":123.406873,"lat":41.868946,"speed":20},{"lng":123.407274,"lat":41.868997,"speed":9},{"lng":123.408307,"lat":41.869097,"speed":28},{"lng":123.409425,"lat":41.869164,"speed":37},{"lng":123.410591,"lat":41.869163,"speed":30},{"lng":123.411008,"lat":41.869146,"speed":20},{"lng":123.411058,"lat":41.869096,"speed":20},{"lng":123.411125,"lat":41.869013,"speed":20},{"lng":123.411158,"lat":41.868646,"speed":27},{"lng":123.411191,"lat":41.867913,"speed":35},{"lng":123.411225,"lat":41.867446,"speed":37},{"lng":123.411275,"lat":41.866979,"speed":35},{"lng":123.411375,"lat":41.866229,"speed":26},{"lng":123.411425,"lat":41.86558,"speed":19},{"lng":123.411441,"lat":41.865064,"speed":24},{"lng":123.411508,"lat":41.864464,"speed":19},{"lng":123.411541,"lat":41.863897,"speed":10},{"lng":123.411591,"lat":41.863764,"speed":15},{"lng":123.411841,"lat":41.863729,"speed":13},{"lng":123.412458,"lat":41.863796,"speed":26},{"lng":123.413425,"lat":41.863846,"speed":30},{"lng":123.414558,"lat":41.863896,"speed":31},{"lng":123.415725,"lat":41.863979,"speed":33},{"lng":123.416725,"lat":41.864012,"speed":28},{"lng":123.417291,"lat":41.863978,"speed":15},{"lng":123.417375,"lat":41.863878,"speed":15},{"lng":123.417425,"lat":41.863628,"speed":24},{"lng":123.417475,"lat":41.862895,"speed":33},{"lng":123.417541,"lat":41.862428,"speed":37},{"lng":123.417591,"lat":41.861928,"speed":43},{"lng":123.417625,"lat":41.861362,"speed":43},{"lng":123.417641,"lat":41.860878,"speed":35},{"lng":123.417641,"lat":41.859978,"speed":39},{"lng":123.41769,"lat":41.859478,"speed":40},{"lng":123.417824,"lat":41.858795,"speed":23},{"lng":123.417957,"lat":41.858728,"speed":22},{"lng":123.418124,"lat":41.858712,"speed":24},{"lng":123.418224,"lat":41.858745,"speed":26},{"lng":123.418324,"lat":41.858778,"speed":26},{"lng":123.418457,"lat":41.858895,"speed":28},{"lng":123.419157,"lat":41.859544,"speed":41},{"lng":123.41934,"lat":41.859861,"speed":44},{"lng":123.419507,"lat":41.860327,"speed":46},{"lng":123.419724,"lat":41.860861,"speed":41},{"lng":123.41989,"lat":41.861027,"speed":41},{"lng":123.420207,"lat":41.861227,"speed":43},{"lng":123.420574,"lat":41.861394,"speed":43},{"lng":123.421157,"lat":41.861543,"speed":46},{"lng":123.42194,"lat":41.86166,"speed":49},{"lng":123.422773,"lat":41.861792,"speed":52},{"lng":123.423656,"lat":41.861909,"speed":50},{"lng":123.424489,"lat":41.862041,"speed":49},{"lng":123.425288,"lat":41.862174,"speed":46},{"lng":123.426072,"lat":41.86229,"speed":46},{"lng":123.426804,"lat":41.862422,"speed":43},{"lng":123.427504,"lat":41.862522,"speed":41},{"lng":123.42817,"lat":41.862638,"speed":43},{"lng":123.428886,"lat":41.862755,"speed":45},{"lng":123.429669,"lat":41.862887,"speed":48},{"lng":123.430468,"lat":41.863003,"speed":48},{"lng":123.431301,"lat":41.863135,"speed":50},{"lng":123.432133,"lat":41.863235,"speed":48},{"lng":123.432632,"lat":41.863217,"speed":48},{"lng":123.433099,"lat":41.8631,"speed":48},{"lng":123.433531,"lat":41.8629,"speed":51},{"lng":123.433898,"lat":41.862633,"speed":52},{"lng":123.434181,"lat":41.862282,"speed":54},{"lng":123.43443,"lat":41.861765,"speed":56},{"lng":123.434714,"lat":41.861049,"speed":61},{"lng":123.435029,"lat":41.860248,"speed":69},{"lng":123.435363,"lat":41.859364,"speed":72},{"lng":123.435695,"lat":41.858514,"speed":70},{"lng":123.436028,"lat":41.85768,"speed":67},{"lng":123.436311,"lat":41.856863,"speed":65},{"lng":123.436544,"lat":41.856063,"speed":63},{"lng":123.436744,"lat":41.855296,"speed":61},{"lng":123.436943,"lat":41.854546,"speed":59},{"lng":123.437126,"lat":41.853829,"speed":57},{"lng":123.43731,"lat":41.853129,"speed":56},{"lng":123.437525,"lat":41.852445,"speed":54},{"lng":123.437759,"lat":41.852078,"speed":52},{"lng":123.438208,"lat":41.851678,"speed":51},{"lng":123.438624,"lat":41.85146,"speed":50},{"lng":123.439073,"lat":41.851293,"speed":48},{"lng":123.439539,"lat":41.851226,"speed":49},{"lng":123.440205,"lat":41.851208,"speed":46},{"lng":123.440837,"lat":41.851257,"speed":46},{"lng":123.441686,"lat":41.851306,"speed":52},{"lng":123.442617,"lat":41.851354,"speed":57},{"lng":123.443633,"lat":41.851403,"speed":63},{"lng":123.444748,"lat":41.851468,"speed":67},{"lng":123.445846,"lat":41.851516,"speed":63},{"lng":123.446877,"lat":41.851565,"speed":59},{"lng":123.447825,"lat":41.851596,"speed":56},{"lng":123.448724,"lat":41.851644,"speed":52},{"lng":123.449572,"lat":41.851677,"speed":52},{"lng":123.450503,"lat":41.851725,"speed":56},{"lng":123.451501,"lat":41.851756,"speed":61},{"lng":123.452566,"lat":41.851821,"speed":67},{"lng":123.453713,"lat":41.851869,"speed":70},{"lng":123.45491,"lat":41.851933,"speed":70},{"lng":123.456058,"lat":41.851981,"speed":65},{"lng":123.457122,"lat":41.852028,"speed":61},{"lng":123.458136,"lat":41.852093,"speed":57},{"lng":123.459083,"lat":41.852158,"speed":56},{"lng":123.459965,"lat":41.852172,"speed":50},{"lng":123.460963,"lat":41.85222,"speed":48},{"lng":123.461594,"lat":41.852253,"speed":44},{"lng":123.462342,"lat":41.852284,"speed":46},{"lng":123.463372,"lat":41.852332,"speed":57},{"lng":123.464354,"lat":41.85238,"speed":59},{"lng":123.465334,"lat":41.85241,"speed":57},{"lng":123.466281,"lat":41.852442,"speed":57},{"lng":123.467212,"lat":41.852423,"speed":54},{"lng":123.468109,"lat":41.852288,"speed":57},{"lng":123.469023,"lat":41.852085,"speed":57},{"lng":123.469921,"lat":41.851816,"speed":57},{"lng":123.470785,"lat":41.851564,"speed":54},{"lng":123.471615,"lat":41.851312,"speed":50},{"lng":123.472363,"lat":41.851077,"speed":48},{"lng":123.473111,"lat":41.850875,"speed":44},{"lng":123.473776,"lat":41.85064,"speed":41},{"lng":123.474424,"lat":41.850422,"speed":46},{"lng":123.475088,"lat":41.850203,"speed":44},{"lng":123.475752,"lat":41.850018,"speed":38},{"lng":123.476633,"lat":41.849782,"speed":30},{"lng":123.477846,"lat":41.849429,"speed":44},{"lng":123.478543,"lat":41.849227,"speed":43},{"lng":123.479191,"lat":41.849025,"speed":39},{"lng":123.479806,"lat":41.848858,"speed":37},{"lng":123.480387,"lat":41.848689,"speed":36},{"lng":123.481434,"lat":41.848419,"speed":30},{"lng":123.482098,"lat":41.848251,"speed":6},{"lng":123.482896,"lat":41.848065,"speed":28},{"lng":123.483908,"lat":41.847762,"speed":37},{"lng":123.484556,"lat":41.847544,"speed":44},{"lng":123.485237,"lat":41.847292,"speed":43},{"lng":123.485901,"lat":41.847091,"speed":41},{"lng":123.486566,"lat":41.846905,"speed":39},{"lng":123.487164,"lat":41.84672,"speed":35},{"lng":123.488093,"lat":41.846384,"speed":19},{"lng":123.488127,"lat":41.846284,"speed":18},{"lng":123.48811,"lat":41.846234,"speed":20},{"lng":123.487844,"lat":41.845801,"speed":25},{"lng":123.487612,"lat":41.845202,"speed":22},{"lng":123.487346,"lat":41.844553,"speed":24},{"lng":123.487031,"lat":41.844053,"speed":24},{"lng":123.486847,"lat":41.843671,"speed":23},{"lng":123.486881,"lat":41.843204,"speed":22},{"lng":123.486582,"lat":41.842739,"speed":22},{"lng":123.486283,"lat":41.84219,"speed":22},{"lng":123.486,"lat":41.841756,"speed":23},{"lng":123.485851,"lat":41.841724,"speed":22},{"lng":123.485635,"lat":41.841757,"speed":20},{"lng":123.485003,"lat":41.841943,"speed":24},{"lng":123.484323,"lat":41.842178,"speed":22},{"lng":123.484256,"lat":41.842261,"speed":22},{"lng":123.48424,"lat":41.842378,"speed":22},{"lng":123.484323,"lat":41.842545,"speed":23},{"lng":123.484639,"lat":41.843044,"speed":22},{"lng":123.484804,"lat":41.843627,"speed":20},{"lng":123.485004,"lat":41.843859,"speed":7},{"lng":123.484938,"lat":41.843959,"speed":17},{"lng":123.484323,"lat":41.844245,"speed":24},{"lng":123.483509,"lat":41.84448,"speed":20},{"lng":123.482811,"lat":41.844649,"speed":20},{"lng":123.482197,"lat":41.844867,"speed":15},{"lng":123.481865,"lat":41.844968,"speed":8},{"lng":123.481615,"lat":41.845103,"speed":13},{"lng":123.481134,"lat":41.845254,"speed":8},{"lng":123.480918,"lat":41.845155,"speed":14},{"lng":123.480885,"lat":41.844988,"speed":19},{"lng":123.480653,"lat":41.844388,"speed":28},{"lng":123.480237,"lat":41.843656,"speed":28},{"lng":123.479921,"lat":41.843073,"speed":28},{"lng":123.479506,"lat":41.842224,"speed":35},{"lng":123.47909,"lat":41.84141,"speed":30},{"lng":123.478758,"lat":41.84096,"speed":11},{"lng":123.478409,"lat":41.840394,"speed":22},{"lng":123.478176,"lat":41.839912,"speed":17},{"lng":123.477894,"lat":41.839213,"speed":17},{"lng":123.477727,"lat":41.838696,"speed":9},{"lng":123.477395,"lat":41.838264,"speed":11},{"lng":123.477279,"lat":41.838047,"speed":10},{"lng":123.477063,"lat":41.837465,"speed":25},{"lng":123.476747,"lat":41.836816,"speed":29},{"lng":123.476415,"lat":41.836183,"speed":25},{"lng":123.476132,"lat":41.835617,"speed":15},{"lng":123.476099,"lat":41.835434,"speed":20},{"lng":123.476132,"lat":41.83535,"speed":20},{"lng":123.476199,"lat":41.835267,"speed":20},{"lng":123.476316,"lat":41.835217,"speed":20},{"lng":123.478026,"lat":41.834778,"speed":33},{"lng":123.479007,"lat":41.834526,"speed":28},{"lng":123.479787,"lat":41.834208,"speed":24},{"lng":123.480684,"lat":41.834005,"speed":30},{"lng":123.481481,"lat":41.833753,"speed":22},{"lng":123.482262,"lat":41.833551,"speed":26},{"lng":123.483043,"lat":41.833332,"speed":22},{"lng":123.483707,"lat":41.833063,"speed":24},{"lng":123.484289,"lat":41.832678,"speed":26},{"lng":123.484936,"lat":41.832159,"speed":33},{"lng":123.485866,"lat":41.831491,"speed":39},{"lng":123.486297,"lat":41.831123,"speed":37},{"lng":123.486596,"lat":41.830755,"speed":35},{"lng":123.487062,"lat":41.830037,"speed":17},{"lng":123.487377,"lat":41.829586,"speed":20},{"lng":123.487826,"lat":41.828901,"speed":35},{"lng":123.488125,"lat":41.828484,"speed":39},{"lng":123.488439,"lat":41.827999,"speed":43},{"lng":123.488772,"lat":41.827515,"speed":43},{"lng":123.489054,"lat":41.827048,"speed":39},{"lng":123.489336,"lat":41.826614,"speed":37},{"lng":123.489602,"lat":41.826196,"speed":35},{"lng":123.489868,"lat":41.825795,"speed":37},{"lng":123.490167,"lat":41.825378,"speed":36},{"lng":123.490615,"lat":41.824627,"speed":30},{"lng":123.49103,"lat":41.824042,"speed":26},{"lng":123.491445,"lat":41.823341,"speed":26},{"lng":123.491861,"lat":41.822823,"speed":20},{"lng":123.492209,"lat":41.822238,"speed":31},{"lng":123.492458,"lat":41.821872,"speed":10},{"lng":123.492474,"lat":41.822088,"speed":13},{"lng":123.492026,"lat":41.822689,"speed":24},{"lng":123.491595,"lat":41.823224,"speed":20},{"lng":123.491428,"lat":41.823508,"speed":0},{"lng":123.491428,"lat":41.823508,"speed":0},{"lng":123.491213,"lat":41.823658,"speed":11},{"lng":123.491179,"lat":41.823975,"speed":6},{"lng":123.490748,"lat":41.824477,"speed":11},{"lng":123.490581,"lat":41.824877,"speed":24},{"lng":123.49015,"lat":41.825544,"speed":35},{"lng":123.489652,"lat":41.826313,"speed":31},{"lng":123.489453,"lat":41.82668,"speed":17},{"lng":123.489702,"lat":41.826746,"speed":9},{"lng":123.490117,"lat":41.826879,"speed":9},{"lng":123.490051,"lat":41.826912,"speed":0},{"lng":123.490183,"lat":41.826711,"speed":0},{"lng":123.490083,"lat":41.826662,"speed":0},{"lng":123.489918,"lat":41.826929,"speed":0},{"lng":123.489951,"lat":41.826962,"speed":0},{"lng":123.489834,"lat":41.827012,"speed":4},{"lng":123.489702,"lat":41.826846,"speed":8},{"lng":123.48942,"lat":41.826631,"speed":7},{"lng":123.489253,"lat":41.826714,"speed":13},{"lng":123.488805,"lat":41.827382,"speed":28},{"lng":123.488307,"lat":41.82815,"speed":31},{"lng":123.487908,"lat":41.828885,"speed":26},{"lng":123.487577,"lat":41.829352,"speed":7},{"lng":123.487144,"lat":41.830053,"speed":26},{"lng":123.486663,"lat":41.830771,"speed":37},{"lng":123.486297,"lat":41.831156,"speed":37},{"lng":123.485866,"lat":41.831541,"speed":41},{"lng":123.485418,"lat":41.831892,"speed":35},{"lng":123.48482,"lat":41.832377,"speed":7},{"lng":123.484239,"lat":41.832795,"speed":17},{"lng":123.483507,"lat":41.833163,"speed":31},{"lng":123.482511,"lat":41.8335,"speed":35},{"lng":123.481913,"lat":41.833668,"speed":39},{"lng":123.481315,"lat":41.833836,"speed":37},{"lng":123.480552,"lat":41.834056,"speed":7},{"lng":123.479837,"lat":41.834258,"speed":26},{"lng":123.47884,"lat":41.83456,"speed":39},{"lng":123.478143,"lat":41.834745,"speed":43},{"lng":123.477512,"lat":41.83493,"speed":37},{"lng":123.476946,"lat":41.835115,"speed":35},{"lng":123.476166,"lat":41.83545,"speed":17},{"lng":123.476116,"lat":41.835534,"speed":17},{"lng":123.476149,"lat":41.835633,"speed":17},{"lng":123.476431,"lat":41.836133,"speed":30},{"lng":123.47688,"lat":41.837015,"speed":37},{"lng":123.477146,"lat":41.837498,"speed":39},{"lng":123.477329,"lat":41.83798,"speed":26},{"lng":123.477528,"lat":41.838497,"speed":24},{"lng":123.477794,"lat":41.838979,"speed":22},{"lng":123.478044,"lat":41.839478,"speed":17},{"lng":123.47831,"lat":41.839995,"speed":13},{"lng":123.478592,"lat":41.840577,"speed":15},{"lng":123.478725,"lat":41.840777,"speed":15},{"lng":123.479057,"lat":41.841026,"speed":17},{"lng":123.479123,"lat":41.841292,"speed":27},{"lng":123.479389,"lat":41.841909,"speed":34},{"lng":123.479622,"lat":41.842358,"speed":39},{"lng":123.479871,"lat":41.84279,"speed":41},{"lng":123.48012,"lat":41.843307,"speed":43},{"lng":123.480386,"lat":41.843789,"speed":39},{"lng":123.480619,"lat":41.844238,"speed":35},{"lng":123.481017,"lat":41.845004,"speed":30},{"lng":123.48145,"lat":41.845753,"speed":34},{"lng":123.481616,"lat":41.846119,"speed":26},{"lng":123.481583,"lat":41.846219,"speed":20},{"lng":123.481301,"lat":41.846286,"speed":9},{"lng":123.480902,"lat":41.846421,"speed":15},{"lng":123.480802,"lat":41.846488,"speed":17},{"lng":123.480769,"lat":41.846571,"speed":17},{"lng":123.480802,"lat":41.846771,"speed":13},{"lng":123.481002,"lat":41.84682,"speed":7},{"lng":123.481052,"lat":41.846754,"speed":0}];

// 初始化速度条
$("#ionrange_speed").ionRangeSlider({
    min: 1,
    max: 10,
    step: 0.5,
    from: 5,
    postfix: "倍",
    prettify: true,
    hasGrid: true,
    onFinish: function (data) {
        if (carMarker) {
            carMarker.stopMove();
        }
        // 拖动速度条，放下后触发： 设定车辆速度为当前指定的速度
        carSpeed = data.from * 1000;
        VEHICLE_PATH_REPLAY_START = Math.round(routeInfo.length * VEHICLE_PLAY_PROCESS / 100);
        playCar();
    },
});

// 初始化进度条
$("#ionrange_process").ionRangeSlider({
    min: 0,
    max: 100,
    step: 1,
    from: 0,
    postfix: "%",
    prettify: true,
    hasGrid: true,
    onUpdate: function (data) {
        //车辆移动的时候，使用JS方法更新进度条，触发该方法： 记录车辆回放的进度
        VEHICLE_PLAY_PROCESS = data.from;
    },
    onChange: function (data) {
        //手动拖动进度条过程中触发：移动车辆，定位车辆回放位置
        let currentIndex = Math.round(routeInfo.length * data.from / 100);
        let vehicleLocation = routeInfo[currentIndex];
        carMarker.setPosition(new AMap.LngLat(vehicleLocation.lng, vehicleLocation.lat));
    },
    onFinish: function (data) {
        //拖动进度条，确定释放后触发，从当前位置开始回放
        VEHICLE_PLAY_PROCESS = data.from;
        VEHICLE_PATH_REPLAY_START = Math.round(routeInfo.length * VEHICLE_PLAY_PROCESS / 100);
        playCar();
    }
});

// 创建地图
let map = new AMap.Map("mapContainer", {
    position: "lt",
    view: new AMap.View2D({}),
    lang: "zh_cn"
});

// 创建小汽车marker
let carMarker = new AMap.Marker({
    map: map,
    position: [routeInfo[0].lng, routeInfo[0].lat],
    icon: "/assets/img/car.png",
    offset: new AMap.Pixel(-26, -13),
    autoRotation: true
});

// 创建跟速度信息展示框
let carWindow = new AMap.InfoWindow({
    offset: new AMap.Pixel(6, -25),
    content: "wangcw"
});

// 生成路线
initializePaths(routeInfo);

//添加监听事件： 车辆移动的时候，更新速度窗体位置，记录当前回放百分比
AMap.event.addListener(carMarker, 'moving', function (e) {
    let lastLocation = e.passedPath[e.passedPath.length - 1];
    //移动窗体
    carWindow.setPosition(lastLocation);
    //根据gps信息，在源数据中查询当前位置速度
    setVehicleSpeedInWidowns(lastLocation);
    //更新进度条
    $("#ionrange_process").data('ionRangeSlider').update({from: Math.round((e.passedPath.length + VEHICLE_PATH_REPLAY_START) / routeInfo.length * 100)})
});

// 打开速度框
carWindow.open(map, carMarker.getPosition());

// 地图自适应缩放
map.setFitView();

function initializePaths(paths) {
    let line;
    let pathLngLatArray = [];
    if (paths) {
        for (let i = 0; i < paths.length; i++) {
            pathLngLatArray.push(new AMap.LngLat(paths[i].lng, paths[i].lat));
        }
        line = new AMap.Polyline({
            map: map,
            path: pathLngLatArray,
            strokeColor: 'red',
            strokeOpacity: 0.8,
            strokeWeight: 6,
            strokeStyle: 'solid'
        });
        line.setMap(map);
    }
    return line;
}

function setVehicleSpeedInWidowns(lnglat) {
    for (let i = 0; i < routeInfo.length; i++) {
        if (lnglat.distance(new AMap.LngLat(routeInfo[i].lng, routeInfo[i].lat)) < 4) {
            carWindow.setContent("速度:" + (routeInfo[i].speed * 1.852).toFixed(2) + "公里/时");
            return;
        }
    }

}

// 车辆开始回放
function playCar(routeInfo) {
    if (carMarker) {
        carMarker.stopMove();
    }

    //计算需要回放的GPS路径
    let replayPath = [];
    for (let i = VEHICLE_PATH_REPLAY_START; i < routeInfo.length; i++) {
        replayPath.push(new AMap.LngLat(routeInfo[i].lng, routeInfo[i].lat));
    }
    carMarker.moveAlong(replayPath, carSpeed, function (k) {
        return k
    }, false);
}


function loadLocationData() {
    let startTime = $('#starttime').val();
    let endTime = $('#endtime').val();
    const searchParams = {
        startTime: startTime,
        endTime: endTime
    };
    $.ajaxSetup({async:false});
    $.getJSON('/data/location', searchParams, function (mapsData) {
        if (mapsData && mapsData.length > 0 && mapsData[0].Alert) {
            const toastElList = [].slice.call(document.querySelectorAll('.toast'));
            const toastList = toastElList.map(function (toastEl) {
                const toastBodyEl = toastEl.querySelector('.toast-body');
                toastBodyEl.textContent = mapsData[0].Alert;
                return new bootstrap.Toast(toastEl);
            });
            toastList.forEach(toast => toast.show());
        } else {
            playCar(mapsData);
        }
    });

}

function formatDateToNearestTenMinutes(date) {
    const minutes = date.getMinutes();
    const roundedMinutes = Math.round(minutes / 10) * 10;
    const roundedDate = new Date(date);
    roundedDate.setMinutes(roundedMinutes);

    const year = roundedDate.getFullYear();
    const month = ('0' + (roundedDate.getMonth() + 1)).slice(-2);
    const day = ('0' + roundedDate.getDate()).slice(-2);
    const hours = ('0' + roundedDate.getHours()).slice(-2);
    const formattedMinutes = ('0' + roundedDate.getMinutes()).slice(-2);

    return year + '-' + month + '-' + day + ' ' + hours + ':' + formattedMinutes + ':00';
}

$(document).ready(function() {
    jQuery('#starttime').datetimepicker();
    jQuery('#endtime').datetimepicker();

    const now = new Date();
    const oneDayBefore = new Date(now - 24 * 60 * 60 * 1000);
    const defaultStartTime = formatDateToNearestTenMinutes(oneDayBefore);
    const defaultEndTime = formatDateToNearestTenMinutes(now);
    $('#starttime').val(defaultStartTime);
    $('#endtime').val(defaultEndTime);

    loadLocationData();

})
