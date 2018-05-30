(ns clj-playground.advent-code-test
  (:require [clojure.test :refer :all])
  (:require [clojure.string :as s])
  (:require [clojure.set :as sets])
  (:require [clojure.java.io :as io]))

(defn string-to-int-seq
  "converts a string to a sequence of integers"
  [str]
  (map #(Character/digit % 10) (seq str)))

(defn couples-from
  "returns a lazy sequence of the couples made from an element and his next (wrapping around the end of the sequence)"
  [xs]
  (take (count xs) (partition 2 1 (cycle xs))))

(defn first-when-all-equal
  "Returns the first if all the elements are the same, zero otherwise"
  [xs]
  (if (apply = xs) (first xs) 0))

(defn captcha
  "captcha"
  [str]
  (reduce + 0 (map #(first-when-all-equal %) (couples-from (string-to-int-seq str)))))

(captcha "181445682966897848665963472661939865313976877194312684993521259486517527961396717561854825453963181134379574918373213732184697746668399631642622373684425326112585283946462323363991753895647177797691214784149215198715986947573668987188746878678399624533792551651335979847131975965677957755571358934665327487287312467771187981424785514785421781781976477326712674311994735947987383516699897916595433228294198759715959469578766739518475118771755787196238772345762941477359483456641194685333528329581113788599843621326313592354167846466415943566183192946217689936174884493199368681514958669615226362538622898367728662941275658917124167353496334664239539753835439929664552886538885727235662548783529353611441231681613535447417941911479391558481443933134283852879511395429489152435996669232681215627723723565872291296878528334773391626672491878762288953597499218397146685679387438634857358552943964839321464529237533868734473777756775687759355878519113426969197211824325893376812556798483325994128743242544899625215765851923959798197562831313891371735973761384464685316273343541852758525318144681364492173465174512856618292785483181956548813344752352933634979165667651165776587656468598791994573513652324764687515345959621493346623821965554755615219855842969932269414839446887613738174567989512857785566352285988991946436148652839391593178736624957214917527759574235133666461988355855613377789115472297915429318142824465141688559333787512328799783539285826471818279818457674417354335454395644435889386297695625378256613558911695145397779576526397241795181294322797687168326696497256684943829666672341162656479563522892141714998477865114944671225898297338685958644728534192317628618817551492975251364233974374724968483637518876583946828819994321129556511537619253381981544394112184655586964655164192552352534626295996968762388827294873362719636616182786976922445125551927969267591395292198155775434997827738862786341543524544822321112131815475829945625787561369956264826651461575948462782869972654343749617939132353399334744265286151177931594514857563664329299713436914721119746932159456287267887878779218815883191236858656959258484139254446341")

(deftest day-1-test
  (testing "first digit (1) matches the second digit and the third digit (2) matches the fourth digit"
    (is (= (captcha "1122") 3)))
  (testing "each digit (all 1) matches the next"
    (is (= (captcha "1111") 4)))
  (testing "because no digit matches the next"
    (is (= (captcha "1234") 0)))
  (testing "the only digit that matches the next one is the last digit, 9."
    (is (= (captcha "91212129") 9))))

(defn split-lines-and-items
  [x]
  (map #(s/split % #"\s+") (s/split-lines x)))

(defn string-coll-to-int
  [strs]
  (map #(Integer/parseInt %) strs))

(defn string-matrix-to-int
  [x]
  (map string-coll-to-int x))

(defn max-and-min
  [x]
  [(apply max x) (apply min x)])

(defn difference-between-max-and-min
  [x]
  (apply - (max-and-min x)))

(defn difference-between-max-and-min-tbl
  [x]
  (reduce + 0 (map difference-between-max-and-min x)))

;;(def x "1\t2\n3\t4")
(defn checksum
  [x]
  (difference-between-max-and-min-tbl
            (string-matrix-to-int (split-lines-and-items x))))

(deftest day-2-test
  (testing
      (is (= (difference-between-max-and-min [5 1 9 5]) 8)))
  (testing
      (is (= (difference-between-max-and-min [7 5 3]) 4)))
  (testing
      (is (= (difference-between-max-and-min [2 4 6 8]) 6)))
  (testing
      (is (= (checksum "5 1 9 5
7 5 3
2 4 6 8") 18)))
  (testing
      (is (= (checksum "121	59	141	21	120	67	58	49	22	46	56	112	53	111	104	130
1926	1910	760	2055	28	2242	146	1485	163	976	1842	1982	137	1387	162	789
4088	258	2060	1014	4420	177	4159	194	2794	4673	4092	681	174	2924	170	3548
191	407	253	192	207	425	580	231	197	382	404	472	164	571	500	216
4700	1161	168	5398	5227	5119	252	2552	4887	5060	1152	3297	847	4525	220	262
2417	992	1445	184	554	2940	209	2574	2262	1911	2923	204	2273	2760	506	157
644	155	638	78	385	408	152	360	588	618	313	126	172	220	217	161
227	1047	117	500	1445	222	29	913	190	791	230	1281	1385	226	856	1380
436	46	141	545	122	86	283	124	249	511	347	502	168	468	117	94
2949	3286	2492	2145	1615	159	663	1158	154	939	166	2867	141	324	2862	641
1394	151	90	548	767	1572	150	913	141	1646	154	1351	1506	1510	707	400
646	178	1228	1229	270	167	161	1134	193	1312	1428	131	1457	719	1288	989
1108	1042	93	140	822	124	1037	1075	125	941	1125	298	136	94	135	711
112	2429	1987	2129	2557	1827	477	100	78	634	352	1637	588	77	1624	2500
514	218	209	185	197	137	393	555	588	569	710	537	48	309	519	138
1567	3246	4194	151	3112	903	1575	134	150	4184	3718	4077	180	4307	4097	1705") 32121))))

;; elements in each perimeter: 1, 8, 16, ...
;; elements in each perimeter side: 1, 3, 5, ...  
;; max number in perimeter: 1, 9, 25, ...
(defn max-number-in-perimeter
  "given the side of a perimeter, it returns the max number found in that perimeter. can only accept odd numbers"
  [n]
  (* n n))

;; mathematically, the first power of an odd number that is greater than n
(defn perimeter
  "given a number, returns the dimension of the side of the perimeter it belongs to.
  can only return odd numbers"
  [n]
  (if (= 1 n)
    1
    (+ 2
       (int (Math/sqrt (last (take-while #(> n %) (map max-number-in-perimeter (iterate #(+ 2 %) 1)))))))))

(defn take-nth-offset
  "given a number, returns the index to take from the offsets"
  [n]
  (- n (inc (max-number-in-perimeter (- (perimeter n) 2)))))

;;5 is the 3rd odd number
;;7 is the 4th odd number, and so on
(defn position-of-odd
  "given an odd number (the size of the side),
  return its position in the sequence of all odd numbers"
  [n]
  (+ 1 (/ (- n 1) 2)))

(defn moves-on-axes
  "given a number, return the number of moves needed from the perimeter it belongs to
  towards the centre"
  [n]
  (- (position-of-odd (perimeter n)) 1))

;(def moves
;  '(:right :up :left :down :right))

;(defn quantities
;  "side is odd"
;  [side]
;  (reverse [(dec side) (dec side) (dec side) (- side 2) 1]))

;(defn moves-in-square
;  "side is odd"
;  [side]
;  (flatten (map repeat (quantities side) moves)))

(defn offsets
  "side is odd
  given a side of a perimeter, build the sequence of offsets.
  An element of the sequence is the amount of moves needed
  to move towards the closest intersection between central axes"
  [side]
  (flatten
   (repeat 4 (concat
              (reverse (range 1 (quot side 2)))
              (range 0 (inc (quot side 2)))))))

(defn moves-towards-axes
  "given a number, return the number of lateral moves
  needed to move towards the horizontal axes"
  [n]
  (nth (offsets (perimeter n)) (take-nth-offset n)))

(defn manhattan-distance-to-centre
  [n]
  (if (= 1 n)
    0
    (+ (moves-on-axes n) (moves-towards-axes n))))

(deftest day-3-test

  (testing
      (is (= (max-number-in-perimeter 1) 1)))
  (testing
      (is (= (max-number-in-perimeter 3) 9)))
  (testing
      (is (= (max-number-in-perimeter 5) 25)))

  (testing
      (is (= (nth (offsets 3) 3) 1)))
  (testing
      (is (= (nth (offsets 3) 4) 0)))
  (testing
      (is (= (nth (offsets 3) 5) 1)))

  (testing
      (is (= (nth (offsets 5) 13)) 0))
  (testing
      (is (= (nth (offsets 5) 14)) 1))
  (testing
      (is (= (nth (offsets 5) 15)) 2))
  (testing
      (is (= (manhattan-distance-to-centre 1) 0)))
  (testing
      (is (= (manhattan-distance-to-centre 12) 3)))
  (testing
      (is (= (manhattan-distance-to-centre 23) 2)))
  (testing
      (is (= (manhattan-distance-to-centre 1024) 31)))
  (testing
      (is (= (manhattan-distance-to-centre 325489) 552))))

(defn passphrase-valid?
  "split and compare count between distinct and original sequence"
  [p]
  (let [w (s/split p #"\s")]
       (= (count (distinct w)) (count w))))

(def input
  "the input"
  "kvvfl kvvfl olud wjqsqa olud frc
slhm rdfm yxb rsobyt rdfm
pib wzfr xyoakcu zoapeze rtdxt rikc jyeps wdyo hawr xyoakcu hawr
ismtq qwoi kzt ktgzoc gnxblp dzfayil ftfx asscba ionxi dzfayil qwoi
dzuhys kfekxe nvdhdtj hzusdy xzhehgc dhtvdnj oxwlvef
gxg qahl aaipx tkmckn hcsuhy jsudcmy kcefhpn kiasaj tkmckn
roan kqnztj edc zpjwb
yzc roc qrygby rsvts nyijgwr xnpqz
jqgj hhgtw tmychia whkm vvxoq tfbzpe ska ldjmvmo
nyeeg omn geyen ngyee rcjt rjuxh
qpq udci tnp fdfk kffd eyzvmg ufppf wfuodj toamfn tkze jzsb
rrcgxyp rbufd tfjmok vpyhej hcnz ftkojm
jnmomfc jnmomfc bkluz izn ovvm flsch bkluz
odisl hzwv hiasrhi hez ihihsra qpbmi ltwjj iknkwxf nbdtq gbo
gjtszl gjtszl fruo fruo
rdapv gaik cqboix sxnizhh uxmpali jdd usqnz advrp dze
flooz flooz qad tcrq yze bnoijff qpqu vup hyagwll
lnazok dze foi tqwjsk hpx qcql euzpj mwfrk
ilb fmviby ivybmf gtx xtg
rpauuu timere gyg wcolt ireetm safi
croe szwmq bbhd lciird vhcci pdax
hnc ykswt qqqmei goe bri wmyai hnc qpgqc pberqf bzs
hsnrb wdvh iezzrq iezzrq rdbmpta iezzrq kemnptg alkjnp wymmz
ngw don ddvyds nlhkoa aaf gptumum ugtpmmu
vmccke qbpag kvf kvf tgrfghb kvf bhpd sglgx
obomgk bkcgo yso ttft vbw ckl wjgk
fli qvw zhin dfpgfjb udsin nihz ovr tiewo
tgmzmph hauzieo jmg tdbtl lvfr qpaayq qapaqy ausioeu jun piygx
jkp guqrnx asdqmxf vmfvtqb tloqgyo ioix gajowri tmek ilc puhipb
uycn zxqm znft ayal znacus kvcyd ekv qqfpnh
fqghur xtbtdd ztjrylr bpuikb ziyk
rvakn uqbl ozitpdh uqbl dsej xehj
laxp haz jyd xnkrb ijldth woy xapl iqgg alpx gnupa ukptmmh
dyiy dyiy ihb qcyxr
wbwkd hdwu zvgkn hdwu wjc sakwhn zxujdo npllzp uyr uyr
fxczpmn cininu akcxs ggslxr riyxe ojisxe
ppbch sampq dnct afikor dnct edsqy pnzyzmc afikor
jnvygtn hijqjxl vsd jnvygtn nqcqv zns odq gkboxrv kolnq wrvd
mroq mroq flsbu flsbu
fyshor xvpaunj qmktlo xoce wkiyfu ukcl srndc ugwylwm ozcwdw mtqcste kpokr
cfh cxjvx cfh cfh uewshh
bpspbap bpspbap fquj mxmn bwls iirhvuk dmpkyt exrn mxmn
tvyvzk ezszod ntxr xtnr och
knfxhy kbnyl knfxhy xhkssx lxru uprh nkxpbx oodolxr tpvyf
nblmysu iwoffs upgof tyagwf aan vovji ajk ywzq oyfi sfulz
aushzkm lcaeki mkuzsah ynxvte rsntd refk pcm
mgguob gobmug dzenpty gmogbu
yvq eepof rgnree nerger fpb stfrln ernger
hrgkbl mzwvswk rsrsbk ieru holco pajvvn ztgsr qkyp fyeg owpcmoj
fowda gmsqdca yugj mcrroxv mqcbojd fjnqfji qdfsc jqs
qnc rvjfz vvxk sjd xrma ucdjvq sbw zydyt dfzww
ocajazv cozaajv tqunkla udwf ecnnmbz lsakqg bki njnda zsdu ccfqw rxpc
qqm qdfya qxyx qmq qfday uqnfttt
rnbirb iapor qet iapor hxkhz dfvzig pedl ybyb
mkgamxg xkniv meb hbzmxjn dhbj zhbxjmn hdjb
ilteux pyutyfx mau lrr bacak
sjjonmn dbbbgs crxyuu jztstgd ezb uiabyaa
tra fle ufzlvf nnaw kec hiwnnlj tei wld iyt syk hjdczb
qmd jtlud dgh dbanock fzp dsjgqru wwvo jwvxwgv xlemfij jcacd
rpkx oxesil snazcgx fly miiyc ikmtmp oefyyn egbw
ypfpeu wldnyd acchppb yqwcaw wldnyd turbz megci nbgxq xkc ypfpeu
iqqv iqqv neui iqqv
ypsxm icqyup zyetrwq nbisrv
viommi toszx dpueq eyy cunjou ffcjc jaeez djefra pxvkj liudlig yye
fhnacbg jghchh ghjhhc iue hwqmo
vbjw lpn cizba ltnsfpz tzoweml irewlc uzckhpd mszal obd
yeos utxkft hflxkfe fxczge qpgigkc ksgr vuumql vhlvv
xzmkv xzmkv krecdi klpem jsbu nwcmik emfzxf cjmpgnj
vtkjo pmiv zou gxo qdiyxsf hwyinjk jhkgf rjq
dyuoc ywiyvch irfgl ywiyvch fxb fxb
tuz onhr syu rqya abkaf bcfx mbknex juwoor zmksl
oheg spjorx ksdy vwtq fxz phvtazk tcze lrxg
hew lbup botaj ltr jpd
dxgc tzinkej gnz hxvvub adsqmc dxgc asgpp rqbdcra goy pmamdua bhiacva
xqv ygb kihxqz vyv pjcny vmyvsdv cgsi nfyx
tqga ssshrw ndq qlbvwh huyd pxbgj qbxk dkkbf jxy chsobw pph
hxl iwph iwph xnr otifm ljhre
zlgvpd kapxpoc dve rklk ogh hgnp rbrmc zzkz hhmcx aklmo
sar gfor nkf hek nkf aql shc aql
dtcrw kfjzcjx qyhi bldson whwdayo mqtgt xhqzp ttqmg
omspdml isze jdl nvwo qrkm wztfg ssfgyh dryj jhp unsmty
jxt cszylng ifht ixtuna azoi xutqlv jtx tjx
usgm azuayp fgkby ezpyq jqwl ezofj
tnhvil nrvg moyrpqs sldx qymoff megflxh pyhqwms xmdw
zomy zcquwnv lzx bvcna yods mjp dgsez
blklyf xokd gpit tiysj yrwfhm tofx
dtig vhdp omuj vhpd
fogwxim qvdwig emdiv jvhl euwbzkg xvxb hwmqo ujdmlp epmykj
sjxll sjxll pedvgb sjxll
drvay gtzhgtx yrt okz nqf
haxfazn pvkovwb pgu tgshw mxcjf pbe nwoymzc mxcjf pbe hydwy jradcr
prjsloa ahylvj okbsj qbdcdjt pmfo pagyoeg vkmhjzt khzmjvt opfm xfrji gyjqyel
lzypt jdbtrad ogr jdbtrad heink
rcoucuq gdxewa rcoucuq whlw zhhm rcoucuq azaqohe mzyli rdvaf
yuag ebcf yuag nsotg qqzuxr jfmao vyucw wmoye
qwvk xemm hgqrr wyxkpp tojndm xlvzypw jus bgnu bgnu nklfwhs
daqi knenmku ccm xkiuy vkexsbc kvvdagx umopitw yaocnx yoakqql mllmsp
mrxgl gywit mfopia ncnsvw vdxek axuiot rsejua nei prndudz mnu
egqn gaa qgen urs mix zbn rhn
ewharq aihy udkdaob kgrdd kgrdd kugbjtj fcef llqb pduxaq wcexmm
dwtiw nelq hppad algxgf gcc upou akm efnb mxmhrud
yxqaa ups okbhgt iet qns tqn rnjqxgp
npmhdm cgds ldexvr typi jyivoqk zkgq vfyxu xgfo
dkwnmr umm dkwnmr okpjw wqx jpztebl eqsib dkwnmr
dxbild wpbup evscivq dxbild dxbild geqp ojfbpl jshvqej
cxdntxs csfocjd pyy tuhws teb boyloz xfw scxh pxhonky
lteucke xrgwy hszgzu hnyrcvb
pfgsgwg dxzh fworek qbstod
usemcrf psczxu gcjtr brls
hjol efxczux bqdn gvrnpey yyoqse gbam ndzyj lbwb bhzn unsezg
bapw xifz blupk qqdk bofvqpp wnbuwyt rnwocu lzwgtt zucag pov
xkre lqvd juf lqvd xio xyg xyg
tzdao ztheib aymcf aorg iyawrch hetcxa iyawrch czdymc ccv
ucgl azlppu jvxqlj pest
dvwlw fuuy mnhmm okrp ualnqlm uyuznba fzyejk yaq crl ctprp
odfq knox mkbcku pxucmuf lpjpol phl
ixongh hfs ruorbd auy qyssl kykwcix aytsm rlj aytsm duq segpqhk
izufsk wedpzh podjkor eamo vqvev ifnz podjkor xrnuqe
twyfps bmdbgtu qye qkwjms
wlav htym vhsnu cocphsj mdsuq vhsnu jflgmrp
opajag itwjhfu purnnvk opajag
hpkopqp vnj aialpt lzrkzfs nwucez nwuezc
mcx hzcjxq zbxr dsx tpknx fva
rlvgm xrejsvn ghawxb efyos xty wdzdgh olahbtn rga efyos vhtm nsr
cni mbab qtgeiow ulttn rckc kmiaju jvbq emyvpew cdlxldn ulttn brhkprx
eykpffp rapik qki fhjgdyu tome ehjuy bibjk htxd vexvag
wrk dpxt gwkuiov gbkif ike gbkif pcd wpj toywyf qzsa aol
yqwzh uujn ujun ujnu
srs ralwxrz yxvvmgp sjhbhk waasid cqtxoxf whcladv jkmaq khjbsh dlavcwh
mdvsjh xaj etvxlsy fxgiy rgjesel rlegesj ptriz ebdyhkp kugxm dxv egljser
lhehwrs mqevb ygmv gri izop qgb ivm
loqqam alojlwg hgen hbyw qlwpun loqqam worgnwk kope
phozre todsknr todsknr ibj mvllsar
wuripy ruwlfbh wukbkey qhq iishw tvtvci xawvxc vxacwx hsiwi ogq
xryq vxwupqa zhqex aquxpwv bnvxrba dtbxki
yvvwh zvsm vqskhp vqskhp ggqqlw bpn wbuv
kqz tdy goqwge ygn jgd
szjjhdk zkpoo nxexz ebicc
wzuemcj oyd qupulju iaakzmt vzkvz
nppahov umm wpzev wxkgfxd owgekp bhhb bbhh dgviiw kdfgxwx wryb
bnc rhes lmbuhhy kwbefga bnc rtxnvz bnc
ani mggxf mcoixh zdd nai hbhzl mes bdpqr
mjn uinoty jjegvze bjgqg yhqsxbt coj obylb hddude xqi rhfbhha alood
cbjzj drmihy tfkrhsd nuhav hihzx bvblqpl tdd szmp gjgfv box
uumhdxd cmwgyf vepr rwqdkj exwk
hwvr ydvw bqefu kghes gvbhp awms iqsqes khgse
mrey jqfw fwvzhps komj dayvs fbui zmtd cofn mrey
dsjds fdpx irjj usndok qcctsvf fgk wvg txwxcl dxs llp zyilwtq
xmkelgk fdukc cye legkxkm wwly
enlny eynln cccku brkz dpof mwfoxcd yftmnqh wpebvyc
ggdn jnysl dsacffw ukj hdae cmzxku
uqhm gcachmn kxndfrl htmfis jfnajz fiqiypr kekho kekho ndcw ckrndub dejfna
keazuq ertql rauwl keazuq obmh rauwl ksrotm
jppp poigqhv repfsje grjk xwkyuh pkx ayzcj hoxzv
yhjw pcuyad icie icie icie hwcsuy wcd yihjh jnrxs
gaug ivvx ceb xujonak hbtfkeb ttciml cctoz
dggyyi dggyyi gqlyumf yasu fwdfa cbb nncn verhq
rhgcw gpcyct kiuhbg kiuhbg gpcyct jlmleo nhumm
wulxxu jyjek hclcp ogob viex wiqcupq
tthu nxgzpid kcnj mss ukapgkp nnc bxjocv qwxs oejwsif aywqtu brahkb
dtde bgvb smu vbbg zhlu
lyo nwjjmep ldbok wgxhto wwuh qfgjknk wnsl
lleyr onha hkwulbm jfg
bybjwd uoxvbh mvj iqfpnxs bybjwd zqtszp wvc lbazjr zkzenja cev
rbuyyr divtslq yuqmyt ajyveb smxsjb nlk tzqhq ims fewg wpjhr gqh
kpewfd beq klilis klisli eeezut
euqh hueq ldoo crqurv lvrwh tmaewp oodl
bqi lzrf jyhvxfh bqi jyhvxfh nbztd lwpdn cuzi
srjylou phavzjd wost uxkaq byh sluryoj
ihrdk bcegkpq nygrs qbcq wyjg dvzme pgzhjl vibg kvv
ijsx iedemek ktlz gtga tbal lbki gtga
vmiaxn kefig kefig vngxz
vrdmfvi qts vlvhq vlvhq dihmq
cfz dyrz zlw qnt vok fwvahg skshbqf hbwozdc ntana jdb uflp
rimbj bxemw sfps krtk umta vnk ewmbx nrlje ymrtqrz mxewb kjxunbt
egnuti ozat eltl ngueti
qtcwoxq rmaf qtcwoxq qtcwoxq
zws gcoa pydruw qsrk lrkybdf ugr wkrxoj nyvf vitwn
tmr hhd dojid zwrj bhsim righ keqlep flzunou
lwoquvy acjowxk tqudk oenvioh nyavyl
rgh dfhgyke iff cpxhuz hui koe iff hui dmukrei
bjiumig lcbmbgh vleipx sfawua rnf
gftfh qwb tfdroe xbno qhgofm vqfoe mux
ljdrr gyfggai iun nju xrucbis mhrcrh fukr obvuqc whlalfe xrucbis nju
nxjmjr egqwg arllu xqaahri lzc ivt uhsti
sqiepba rcmts kvesv nvp
tiksw tiksw rjni gbhvzm ctbq zuqfyvz
ibsnm kfka aoqigwo sqouih rxz
jmymq lxio adtmk umyu sxvzquq bporqnb heol fow
mepa eckq rqviawv dkqoei ifmngpp jiava rtklseu
yuycd jiufjci yuycd uowg yuycd udq izkicbr csxobh
nwu tfsjavb rruoxbn oepcov elxf rruoxbn rruoxbn azglwth jcjm ksqiqpv
dthfwip zqnwa zqnwa zqnwa
gso wruece ufl crgnlxv vllsm dpyfm wpa ctxko
wvpze seodz lpq lpq pmtp wsxs ffppx
yfxquj phvjn rtwieq rtwieq kgxztyu vbjvkc prqqd lyzmdo ojbrt ojbrt qiqjz
esaezr rpggiy jey kbzrhu uthus osr xxaiijd qfxlf auhzbx gkigoqw
yfhcj uvgck cds gjhhrg cmempgj yfhcj cjb
yxi voxvtuw unwg jqqm
igvjr ljz rus sru gbjtjt qfeg ztu zjl
leof ocxns hbkoysh hbkoysh leof
hab lyxmf yhh qeks fwhfxki xmbcak okqjii nfgzyg bhtfgdj lpmjn
mgognh tad herere lvwnzx ixwqs zphmuuc etdjz kczsf
mtej rlolsnn zbl uykek dpkan gmz etxtgj
mihuieo emjgbp jgks mihuieo iexrfw mjdnr bvp mcuzea xkbusvi
jvqpj bwt jvqpj bwt gxr
qpnd fpt tpor bibbpcg hmvguez wqc afl ckviua gpi
dntmcg jglm sxtnu sxtnu sxtnu
fzkbptw cbfwo ozvwov wbv gcdd izqo ovwzov lolewo xikqpw
nkxyxzd kpn datf fki werq mwidqx oiibor zizcjph
xvgyxym zor ijoy lvwsf fjuara idvvq rreit mqyyy ctio tzwqqhj rnpee
maqkfpk maqkfpk xukg sfdmnlg xjopvr xjopvr irf
liujcd vnlkouy dxkwc gto vhjvtw
swhqhj cas aupsd swhqhj cas bvbooii jquck dtdm
igh iqicicf ghi pcxt srcrjx gmf gyscphv
drplj drplj wopgpnk wytag wopgpnk
zexe ilcqoh qiefb txkuv lirfzv
ovvpn ovvpn uqeurqx uwzn hgmucj ovvpn sjxulms
rox silka irhsvym kutus otasof tdneav pcagds
mkja omu tyshbfq onp trxs lxa tftbv bnpl djhnc zdqfs muo
tjj rmmqas cbbkxs qio pikk ykyew gxlxt nhsyl ykyew
frcprg njrz oaxcmhc qben pedm ecvtga nzxwpb ior gaklot dpem
zyt kncau spoe qlchg sqys wkpbng yflju qlchg vkve bzadbpa
qtq pkaicl qtq mfkfqvr dnleiq brrjxsx uoyxh pkaicl yvmlug
firwy imtlp ywl qfa dqrbazz ztzb pcsbwhn zesmlag
ivey ivey mtvc mtvc
lhize acwf moa cdeoazd voktshy qmvqq jvmuvk ljfmq tsanygc
xreiqkc aawrovl pofcsg xreiqkc xreiqkc
cjbzvn ozds iniqu sdoz gqmki bablvll krs vjzcbn
izsod htkeqz entxn qtns prpcwu omfnmoy
kwfb tctzda aztctd tadtcz gyt wunbcub ydiwdin xxk
epnl ijcp giq ltfk zjcabve zfksmz epnl giq xxxbsom
ulyukpa mdjsbn dydko uhkdt qms aaaj hustlwu
zlsbu ohx jcwovf egf zlvpqgx qhejm wrywdmw
uhxqrzr mmu kjxcalj unuohiq rri yzngnb ikvlxry mfiym qbksdx
khqciz som yklmm jceb khqciz jspy jceb
ncwggv njvi nqox krtsn lnm
bgtqme xaxcoq qbtgme obqual vorfk baoqul lgrb
jli tsbb nlxjc pkwzmz dlxrj hmho gzguko ilj iyaasm
wlmw grkumg dynwtyo emxhhqr huluk slpqu uhqcmd absmr ufirmwr
pbs pcammxv dplfr tzvmav nccyy blvyq ffhnz bccutq
hgge ghge vxmvz hqxgjdg zab guo gheg
ylj bucoyoq udndc wpgyrbx ueh udndc gxdsdh hdoz wwgqlg
cjdeh gttyqe kdkm ltzd lfeozse quvjq mnwhokm kdv oojxm nxt
mfkzus knqxt saxkqww njx zumsfk sbmcyad cpt agvbuv
tukn vyco yobvsn bzgnn klrnzy kea thzk pxpwq ryfff nxzm
ylbm lxlz lybm lzxl
wgtxoij zad slgsi cvnxfg iomswwl vmx
hkm yinhnkj kmh kwkw kayknck chur styjif yknakck
rtfwhkq rtfwhkq zsf zsf
sldq zlntr ueegiw kajivqc ozcbm ceft snvugom pdyc elppeed nnqrp prwwf
lhk xjonc muc tudag tsafx mmivb dvrjbp qgrew
hnzer fbgqp aazta aazta lxaz lmgv aazta
victgxu victgxu mlpd ummrnbx cazjgnw isxcyp efy zfa cyusj
gyojxo onzq gyojxo uxufp awi ilhl wefwfxr gcjlt tmliynw uxufp pdcnxah
wjwachn xkuhfbp oky oky ybaeqkr rbuix yreoaw wepmye brvon aasb
kiidorw vxtxiqx wtqvbrv efdth isel qbom vcssyc vxtxiqx wtqvbrv riafzsw mqzsj
eurpjd vkhdamt tmfx czeoot hiz ykz lmixzq tfur jhzr
ipuftpj qbll sqkkdw fwncmiv bri oeeh lehd ioh wag
suima nanngc imrmc krq atxdo woy atxdo akev qlr aezco qlr
cfc efwbzck ozkmcxv moczkvx ccf
bnekky iakrk sask uwgnjp iyi rynev bdnas ldh kass
sicmw vvjbvv cap nsumc xgvrlm wsoo uoqdu psykckm
ugg mtr wnzhmmh tjxc ehwnji lwhu mdsckk yvmk enubrqo
grb oxmxz ohu ytetedv ssx apzlppg fdkamm sxofc jdt ynmu wyejok
umoep rbyqm eqfk twqnog cptbbi dragna ngqs ffb cexxnc rbyqm
utizi ormkel wvwur bdx ecelqbv xiccama aag glfvmj
znb rsuqoa uxo svc
obs lbifa cffi catpd
qkxwian ajlzjz wewduzp bbyv qmt fsr qgiu epinp ghmf
hatg bfgmb aght ghat
kuq inp dun cknbun wmwsu drlmmg kyxc bdl
bddybth swdbf jhi fva qpobio bjwm wjaztp jywi
mgckz vhveu zkemhp zdf xtiqqew mlx wazgd
umbjq pya lvvxf jeavij rhrxvew bwjqgpr piz
xaycpwo vjcuc qksc yuixhni sfbfb dydyaq gdfvb tggg xidphvf bpjdrl goskxym
agxfoip gguif wvo agxfoip ntkbaw fbyggy ooft zxih
nzvsu ffwq uxvfbl qrql olhmhom qhdltg ymwz krtndtx olhmhom nfsv krtndtx
qdp jqk ustz xjripzv mnk grnodk pjwdsj uug zqxjqj
mufrcox zunisfs ocvcge acamm xua vor bsde kxr vor kxr orccxx
ncycbp anvcxay bmm wndmeaw oso knmk mmb wamenwd kmkv ppdd
motdcn xzagzwu vuzt utffrn yuqxzrh uvzt ujttq
tauoqy coiy ybesz tauoqy wpmr trquyne ahxbj jzhems dsdy
aczq ypw pgmzz srfn quatjgf
cih ypapk bfxvr euvhkk gugru auhqui
vyf pssgfvy dnhvbfl xpacme dnhvbfl mzdv iynq hcqu
lbzvbu hhxiq hdfyiiz iyzihfd xhqih uzdqyxr
iapbdll vdr cprmrkk vdr dfjqse mlry flpqk vdr
grrfkq xcpxd grrfkq dxc bjpr prvwh swoc swoc
bopo chvwuhf qhd ieesl xey ieesl fnjcbe
kic fyq hsucnu agwyl pzzmd hqksh psw
mxf uau iti lcoz lpg zbu ocre wqlocmh mxf nidqj lcoz
bypmix ptzxgmf xmtzgpf hrvzzq
lbfw zwusma lbfw tuyyy
lrf uej unswvh obgsb npbl zajr kenea uej qnyjcu wzufim qpzkgya
qcrxj llyu kligt hlm ehwtbx dda lgsvhdt xewfcv uikn
nfzjx izqdbq mfbxs imiuc yqxb xlmvix izqdbq eflqfq wku omgtuu izqdbq
lasdwg hiy btzt eefd eyoep icn nnmhg otml rek luixac nyzgn
vekteds utsuxdx utsuxdx vekteds
feyov qrij zbebwg ijrq seplram wttkwm zewbgb kzuhuh
dmkgtv wohgqo ddtqmv zatahx mym hqowog tkmvdg
vhha wjrmuyx kqh vyyrj xzchbi ejsdq orlxg vyyrj dlrc
yetngqn zdtuqox hkarjei fqpsgh eaqwbg zsssog ghb gddqqzr hbg
obldb zsrhz zxp uxphnev mwnbc pfjft fms xwslk vjm fxy
nfij dbfykv ttq gyjgac igxuyqi gtiioqx ilhdex dbfykv uyp bdiwya gqf
pffzruz vogfosh dcs wje
pohhf fhpoh oon yyz
xxuam afwm qxl lnt syyr bwxhhf sozauq shlhfmz kwnn milav ochq
wefcqrt gejw cwerqtf fttf gjew
jfsvnmr osca epwtle pgfif sxom
exlfzmq nakp rgdnx rrcvth vhrrct aajjdrt ryyg dsozd jdqlqj pakn iruv
rmcvo txszcs xxhyxz hbsozk wshkocf rmcvo rcbnt
kitz yjgney yvkymef nauj hmllsgl kyhm kqr pzsu rcf pzsu qpte
cdinpx bfur mkj naz ihkheyr nohhoe
ylris xeqcgup wap bbfih tgfoj
ina gnlnm zyeqhij cudfuf ipufae bvkdzni aat teqsg cudfuf bjokrbl teqsg
aedx edax dnfwq qndwf
rdngdy jde wvgkhto bdvngf mdup eskuvg ezli opibo mppoc mdup zrasc
qcnc iaw grjfsxe gnf gnf
zbjm snznt zelswrk gkhlnx dqxqn qqxnd dmro
zisecvx ztezof uzbq otnrtj qsjzkwm ewvcp rlir bfghlq tgapdr qxmr
ipnqj opjf vabyoe wkwnd
wyf mfqxnrf apm snarf jqu aaghx pwecbv lvghayg
acncv jmmbwlg oiphlm ifuo cvt
pvmb egansnd zmh gcuzzci rrxpslv ubith
uoleptg xbouzn xbmg cfh cpn wpqi xbouzn xtxis sxzpns
rilybri kurbpq vfmjpck tjyogho hfyxad svfofx lfbbhxj khaerfs iqr
seaebgz wlmtkre qguv qguv wlmtkre
sgo edkxya zdqgwtt gxu nibuu rairqoq mzxli dci qsv
tsol mdhzqr rmaqnru ggvcq arbwkn hlkcnj ljkcuof
mmliphp ocup puoc eijjv
gmajqpb ijki ijki kvz
pmqss unhlpcj dlkll nuhlcjp expe tlurzmv nsy vlumtzr tgseozl
gkvaoni hsba hsba viuedv phyoclp fdq phyoclp febld nqfs
rxvdtw abn pntv qrqfzz slsvv abn lrxix mnu npot
ghlfjp woy xwkbmv bkahpkj jve cncvk jvdype fwgvoju yrkwjp gwfvln mvkv
kmluh mie bby fwer chsinb ojglqr nqk mie
yzmiu igkgca ybnsqja jpfejtp yjddy xsosxfi ingx qwuhb emrkwpx idqjmmm
btrllw mphm dkvo ewdl dchcul yah btrllw kmqi mtvgk wtb
hxsgard yuikc lykt tdee adprp gpougod klnzk mzsmlb
hdn znblw ifoblur bwzln dbv
smofpbs vjuyiro llk lfzesga tybu tybu
gffnpug xaup iqiyz fjkpnkz drrk fwyxw lwzfskz gslwpmv vjxylva tbkyo nib
evydmb nhwuiiu fkerq nkgbuyy uclrs ydjgglh xhotwbm riirgzt
bsub eavbt uvd dpzwyt rhn khrbptt xszckc djnfxju axofhat powmso nvdffrv
xtuykl fjz mbikc xpnx hmey fjz fjz
rkls nwdcsyx rkls rkls
tygml untequ ybdfumz nqffbq uipc sove hfnqj
ytecew vven koqn royynd qsn ksl qsn sdw
hknlw qwho whoq oqwh
lzmmtqu qvhyeo cnofuj utpwkjz gnirz yhhu aodbnd
zsr axw kwtzcv tydzo kwtzcv lkxsm
rbjtqe nihifd gvdxd bpxzy rxteky vgcgllv vbbua anygiup rqo
dpd wblfwp wblfwp wblfwp ygahc tqjbaq
gsw gsw pacgj xmrcz zmxhmch xmrcz
pdq rhe xqmq lgpkhg fyffrot ovnqh wle
tbjavke ypzzrj jizx gdxoh icjsat otfh fmygumv
snch nxlgjgp jeyn sxoqfj jtage jtage iuice
rtb coefuj grwg grwg rtb krhqnma vfhgbr
vhegtl btorwxg szcev kbvkx itsk nlzpbed
hiukrf ilzkm yllhh xsgwkdp zyy kjbv
rfcg tdorci zcj wzftlv rfcg rfcg
lgbc lzizat vsno pau nvv vsno bbr lzizat qhtb gwp
sfwnio tcugjk bsfsz ykyfwg ibkap fsrvy mygk kzunawx zyhyh
mpavlh qps bylh lttjkz rqabgk vewb bwev tlzkjt gzrbxga ktmso prpkj
gpf ims ynh ffrs vpa iemp gofh cgbauje
secys qks mcnfhwh drog kqs pajy zoltkw lfihnb myb ioxptu
ytq nrta ouk ajqblf yuwwcd zdy blyoxbw dakk nvgi bzrhzaa
nkoych sufiia xkdvw crtldee zycl qblab egqhr qblab
nllno muxaf vds qjnitmw zkpj wskyhft kmqct xamuzpw qcai cdjtbt kaxv
qzdytpe osr fuw osr qzdytpe whperd rydwdcl knoa
zkdznhd peh duoygr zamrgl irnvj otpe pltpq jdkecg
byzgw rece iigdug ehif tpgje
ccnn foqdran gbctca tefdjxh ntcr rjciii xip xlss crl wvvhzqm twyohf
dqyii milqqc qjgkojp qjgkojp ryde
tdkyj tbrcud tsba vqtmb cjwxnf
hqhmq wemvrce nagig pwnw nagig epg nagig vlsi
tqgvw luoplw hccti npjm rytdruq cylrsun rytdruq vjsbjl rytdruq ppti
itgt tuwc itgt rvp itgt tigns eipl ksmru
pdw wdhtkn nbdbpn wff zhuuipg rvemv qxr
qgkwdq cjilayh ymeks mrpuzai dwgs stfstgz ucvqhb yout oiq
vpxik ypfr qytimvu qms oxbmw ppyfx
fwwidn gdhd pyuexk snsz iwndfw
lfcb sllxjna lfcb hpzahfg mmvgaa svny jhuzd
unyg gicmzd fwc spkciy toyq wjupckd vzzx iuqgka ytqycb pxsufj
goj tnrcml eyizngj txa xrkiw zvu igduz
wek xrrlkna clyof rrlnxak
cjm rmyuku vjom gtf
buk cfae awstd dywgqp hxo wcxvf laihqw xdqfes wdbh qceh uzlwj
sudguo dxwplto rlebdh bkamu dxwplto
crwkyxm yuz kjtdhom crwkyxm
trhc sduorxr aizfryh rsudxor gbyc
pczkyl bptp qnn nxmpwsx udrg hhlb rubtrmx twzodlp xygnht
jmqct cden yfajtkz fevcw sxonbxz sxonbxz qkzkm hhngr fbv
sdsnm mwvicr wypfi cty ndbowr woiz mrauwzd qlno mwvicr
vteyo fng lvr lxytn txpj milg
wjx ahtmgo cgwcaj kaxae fhlvlqf
ezj eetqhzu upwda iiefwlk vyvby
imalvy yeghqe jwcu mvrod cwju
bxnmsa yhfu npsdar tsbri hfuy sirbt oofxmy
fkndt elbjtn vepqtxt elvpf fpelv bzkgag qttexpv prblwb
rmq iqs yvprnyy iezqrzm wlqsrr
yviovq lekxghj oey qwhzj lxknxw qiyovv ksnt jptz
tyrg cifxt hugqf tyrg ffuiv jmax qyw fozfosq ffuiv
nmg rsl jpzazd qbtlf yxqtsj czwmdfd bamge lbjdof uqy jssc
cbx boozjip pwgvzlq rjz kxy kxy hszacok fvsq jhnir cnsba gafz
sbcuxb wfur nnnfqjj fdwg huhe sbcuxb
icwk qelbxs uevp qped zsnhh wpuok wddxsln ftnzupr ruxol cgxjb jbhh
izcp htykj xxmndoq amnspe htykj
vverol oixwlny vqd tvfzu henc gnyrwr
ytxio etytsx choynep zqapo hfjit
lkvgr oyzfa taiqr jok djatvy ckif tmdw oyzfa zroy
jlgpyp kkqysg oqjki hjohoug hbhta muilz zft
sumfyu wftcu bwwdcy lezimwa qwvxv zwh mqyv bmfot aii torcol rnt
tpdj xrw ccsbnh fhptv fwkxjfm dmqaokd bjci
zxi vmf vmf dpyg
sfzxysw lcms bkojtv bkojtv
opywo qll ipkitr mtwp tudrr svhyp huz bxsdpn xomfy
gkod luo qrosbp orbd rpsjzyd rlh gdok tze
nusiuq nusiuq zeys ahufexc
veno jntg avtmtdn qojxru zegdcql odfcetz pgehau
uqun vigjm ykac ozlelj danmji bibugox
rpuozh ajwru rbvuevv uhzsq
iawoe tyb aewio ymf byt inijv ctu fcys micsgzl pbby alt
gktyxp ris mqpfm bkqsfl nrg idbbcxg jhcf
qibt invvv qibt luitx rnm eby hrfbmwl wnap sgkzvb qlwc hrfbmwl
jwkv qecsjbw lycgldd wjvk tjcp dycldgl pzrvr zrlcf kji
nzsrmiq nmhse ilivrk kqv
besmyzi imkgpt iekbjax abxeijk uvzs wwv
jdocl uki ltswp tjkljc ymce iuepze qygqxzs tei lkry
hhyfy gvzd mqksxlq czn afe mesnag eep frwgekg mqksxlq phpy
ehg connnza ekt ddgokw
mpbsoms uzhzl xevww ztt uzhzl
lftybr firc awsud dsxdkk ltf ipjv dtx lcymth
vkcpb gxtxq yioeq fexj xxgqt
srvca fslnnvf nfmkpvt egw wemumq jie vznf dzsjw cukf kcvyir
yxjkl lyjkx jyxlk kgc xtz
tpoe xzov csp leleoqo noyre tdhf cyib sjgtdx raehdw nmcxp
qvt uhznqe bpvos vtq ddlebtd tqv
xlw utsxs gpia rvlvnts elkxr dddihy tnrslvv ibf wlx bxg
cwqnnrt rkkqyf dye yde fzl pthanj
boc rqjenpp xjqte jteqx pvoofc pidqe ruoucy gvnro ognrv
qhalb gnazwc fhl iuti
clnbjfo nnfs nnfs heymvr oarew oarew nxu
lwtrotg hiaxwj ymzbly nvhzjhj zlsaheg nvhzjhj ymzbly
rrvi tsjp tsjp tsjp killji
rpx hiclj cmwq ibhj nfd
pvwymn iebkd xmpw vuhhkap ksw zigzy mzzyyxy rmuh iwwhea cglfq
rlwelgy sffml jin qsdzro xlsty mgqzuu etxjuo emzd jgnoyq tkjuy vfvb
tkctdj hhkuc viskmy obw
zvjkuj akeky ikj jqd hfhzbwe bkc
btev nrdo hcyiuph stf qharfg vpmel mpfz nvs ytgbbc
ieepn ndueuw svmdr tcvumw mceyrn mrjwhyl tbdj mgrgvz
uxrs ckyi xpmqm czzrkl cjp
nlliwd wrqkrkz yjmng nlliwd zirde hcjjn wco ysf mgl
dxti lcahe ommare izlwf ramsfb nzgfvo ijvm fwymrdu bndq
isxy jpvuzu tdduyhw dixp cfa fkzbteg ytoi kepk ysf yqcpi
qmeprfj soqo ncgeor cqsuuj grzy wogxy vyblnbg slvtry vdols kka
ltykfp gtzl olrp gxend vapee deq
emywfbn dbfiut rkt wvwe dbfiut bwffhea yuzcxv gogpicp wvwe
vqvmrp ofbk dlfabd jwllzxk obx vqpwjj umvng tqwis fstxy fstxy
miha zgvyux rmraszo xwf
kjaagk btm kjaagk wkewjrg kjaagk
lbmli aizs omrdr gzktnx asiz ptanzpa xlo ljre ckyb wob
svz dlk rijagg avxmg fkzwhk uro gegm
dzplum temdw jqnm tvxcww bmg tftttpp deuw comxey xfimzjx caluczi nqn
uwvhxa ztkd nlsdyt vihl julkwwv uzch dwakhs
wkhuihh ycrc cxff vzcfhpp uegfd gaok kcnvz lhzogq lwa tyrypvu
idp zmrrzp zmrrzp nktp xsnx rjsxn
eybrnib ivgntl vaxsbpi eybrnib
nzvnq xvbfa pbhwwh ylju runvsj imlx vztesn
nfdohd nfdohd gtevnky pivjyct ihvd fzcsrq lko fmqk
kwpkks ecikxu bcxswlt qvrxm sbcqmh
kdjrmj piuh kdjrmj vnaf gyedkg vptxgm xezssxx zsg qjzpo zsg
oqo sley aqx qmpqb fgmylbj egd zivj kepxizv kuakyn lunbnd
hmcf hmcf xlhgc hmcf cdlm buofnx
onjcj yluonz kzmk phqo phqo phqo
ohaafy efl bnkkjww wwjnyoj dxeaig ywnjjwo slk hrbebw ohlyju elf
msohiqz aunk njki bfktdgi htmyrj mgx
numlzrl rmnlulz glb ltt fhbajz gqxpu
gko hco oai ryq xwy sdqosft spjkiu cxfhg ycwpglh noy rah
btzpjem brpk vqr atxu rhlh rqv jmg fvyus
phmxxgj ejx xje qtk hsb kqt npwj gqt
hujyjp nwmsd ant zipuya lrkahww uwqal vzlo qmbo twkjkse ufivi
zfbnyz fwvh xrnrw usn zin daq iwjzj
yykyg iwypfy hehqnl cjvk cevdrec
gui muuto wsta glqmx gfo rdmbv mxwz gffzt eejpw gion
lpng nduid iqbpu nduid knrqd
xwxn oefpckv gjaua ugaaj gjuaa
qxk aeql trqdmqc crzlinj crzlinj trqdmqc rijcne ewyf
rfv qmbe fvr bmeq
upqyfw lowzq wpen upqyfw gfskbil sljuzh wpen
bdcara qyhx rtaez qyq gbyr
evzls qxtxq clzd svbgqi zxlzgss vtrre fko eebo qjyl
zaapeo kpwhz tygknau nyd pch trp xqe
ypzcafg rnqmbh qtteg sncu ssojhhm zonfym thir xmgheb wqj gpjg ssojhhm
wvcwyn xrf muozyya lasdp xpjgu kpqv zkiihiv ifje cbdlavg xbied hfnaa
qqqb rettz rycukl ihpkhh
dnxzxqv znb znb fbxj azxtezb xvxa
peqkd xlzqkov esgnw ucku hrwpfxd xtd vnig vlmfp ajte qswr kqoj
dpwy oavzkk dwyp ehij upqxgii pydw
amfc hfv xmqa nqvn cal rqmcq oej amqx cla ntxj
hqhhe qkbhwli wmhlcq xaczs peywuo
vcr xfv xfv kymo qpszwzo xfv
nmrbur tswo xbo ljlrzo bmhpgc pev zovkznz lok wbbhtkk
tojj lxqgr rhjavrm ndsdup gdbjwaq cqpnl wfaxivl rfry ryfr udspnd
beffod sknlph amb feobdf
mldgn jxovw yuawcvz kzgzwht rxqhzev fsdnvu vluuo eycoh cugf qjugo
tlnd qcxj ker fdir cgkpo nrqhyq raef uqadf iahy rxx
mhvisju lhmdbs tcxied xeidtc ujry cditex gvqpqm
cgc jazrp crgnna uvuokl uvuokl uoiwl sknmc sknmc
rvbu czwpdit vmlihg spz lfaxxev zslfuto oog dvoksub")

(defn split-on-newlines
  ""
  [s]
  (s/split s #"\n"))

(defn split-on-whitespace
  ""
  [s]
  (s/split s #"\s"))

(defn remove-first
  "removes first occurrence of an item from a seq"
  [x xs]
  (let [[n m] (split-with (partial not= x) xs)]
    (concat n (rest m))))

(defn permutations
  "permutations of a sequence.
  in case of duplicate items in sequence,
  it includes duplicate permutations"
  [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s] (map #(cons x %) (permutations (remove-first x s)))))
     [s])))

(defn string-to-chars
  ""
  [s]
  ((comp #(map str %) seq) s))

;; instead of intersection:
;; lazily add permutations in sequence,
;; while verifying all items are distinct.
;; when a duplicate is found, stop. 

;; taken from http://grokbase.com/t/gg/clojure/15596dgqrc/opinion-on-take-while-for-stateful-transducers
(defn take-while-accumulating [accf init pred2]
    (fn [rf]
      (let [vstate (volatile! init)]
        (fn
          ([] (rf))
          ([result] (rf result))
          ([result input]
           (if (pred2 @vstate input)
             (do (vswap! vstate accf input)
                 (rf result input))
             (reduced result)))))))

(defn take-while-not-contains
  [a]
  (into [] (take-while-accumulating conj #{} (complement contains?)) a))

;; generate all permutations of all words found in string
;; (mapcat #(permutations (string-to-chars %)) (split-on-whitespace "mldgn jxovw yuawcvz kzgzwht rxqhzev fsdnvu vluuo eycoh cugf qjugo"))

;; (take-while-not-contains (mapcat #(permutations (string-to-chars %)) (split-on-whitespace "abc 123 ab")))

;; (count (mapcat #(permutations (string-to-chars %)) (split-on-whitespace "ab cd")))
;; 4
;; (count (take-while-not-contains (mapcat #(permutations (string-to-chars %)) (split-on-whitespace "ab cd"))))
;; 4

(defn same-length
  [s]
  (=
   (count
    (mapcat #(permutations (string-to-chars %)) (split-on-whitespace s)))
   (count
    (take-while-not-contains
        (mapcat #(permutations (string-to-chars %)) (split-on-whitespace s))))))

(defn intersection-between-permutations
  ""
  [s t]
  (sets/intersection
   (set (permutations (string-to-chars s)))
   (set (permutations (string-to-chars t)))))

(defn word-permutations
  ""
  [s]
  (mapcat #(permutations (string-to-chars %)) (split-on-whitespace s)))

(defn first-seq-then-word-permutations
  ""
  [xs]
  (apply concat (map string-to-chars xs) (map #(rest (word-permutations %)) xs)))

;; previus code is correct

(defn while-not-contains
  ""
  [s]
  (take-while-not-contains (first-seq-then-word-permutations (split-on-whitespace s))))

;; better algorithm

(defn set-of-sorted-strings
  "splits on whitespace the input
  sorts each resulting element and makes a set"
  [s]
  (set (map sort (split-on-whitespace s))))

(defn passphrase-valid-anagrams?
  ""
  [s]
  (=
   (count (split-on-whitespace s))
   (count (set-of-sorted-strings s)))) 

(defn filter-valid-passphrases
  [str]
  (filter passphrase-valid-anagrams? (split-on-newlines str)))

(deftest day-4-test
  (testing
      (is (true?  (passphrase-valid? "aa bb cc dd ee")))
      (is (false? (passphrase-valid? "aa bb cc dd aa")))
      (is (true?  (passphrase-valid? "aa bb cc dd aaa")))
      (is (=      (count (split-on-newlines "a\nb\nc\nd")) 4))
      (is (=      (count (split-on-newlines input)) 512))
      (is (=      (count (filter passphrase-valid? (split-on-newlines input))) 337)))

  ;; part 2
  (testing
      (is (= (string-to-chars "abc") '("a" "b" "c")))
      (is (= (take-while-not-contains '(1 2 3 2)) [1 2 3]))
      (is (= (permutations (string-to-chars "ab")) '(("a" "b") ("b" "a")))))
  
  (testing
      (is (= (count (word-permutations "ab cd")) 4))
      (is (= (word-permutations "aa aa") '(("a" "a") ("a" "a") ("a" "a") ("a" "a")))))

  (testing
    (is (= (passphrase-valid-anagrams? "abc def") true))
    (is (= (passphrase-valid-anagrams? "ab ba") false))
    (is (= (passphrase-valid-anagrams? "abcde fghij") true))
    (is (= (passphrase-valid-anagrams? "abcde xyz ecdab") false))
    (is (= (passphrase-valid-anagrams? "a ab abc abd abf abj") true))
    (is (= (passphrase-valid-anagrams? "iiii oiii") true))
    (is (= (passphrase-valid-anagrams? "iiii oiii ooii oooi oooo") true))
    (is (= (passphrase-valid-anagrams? "oiii ioii iioi iiio") false))
    )

  (testing
      (is (= (count (filter-valid-passphrases input)) 231))))

(def day-5-input (io/resource "resources/day-5-input.txt"))
(def day-5-input-seq 
  (map #(Integer/parseInt %) (apply list (s/split-lines (slurp day-5-input)))))

;; (println day-5-input-seq)

(defn offset-to-index
 [offset prev-idx]
 ""
 (+ offset prev-idx))

(defn map-nth
  [xs ys n]
  (map #(nth % n) [xs ys]))
;; (map-nth [0 1 2] [3 4 5] 2)

;; To get the next offset to use, sum offset with increments
;; (reduce + *1)

(defn inc-at-idx
  [xs idx]
  (update xs idx inc))
;; (inc-at-idx [0 1 2] 1)

(defn strange-inc-at-idx
  [xs idx offset]
  ;; if the offset was three or more, instead decrease it by 1. 
  ;; Otherwise, increase it by 1 as before.
  (if (>= offset 3)
    (do
      (println "idx " idx " gt3 " offset)
      (update xs idx dec)
      )
    (do 
      (println "idx " idx " lt3 " offset)
      (update xs idx inc)
      )
    ))

(defn repeat-zeros
  [n]
  (vec (repeat n 0)))
;; (repeat-zeros 1)

;; idea: maintain a second vector of same size
;; to keep track of the increments when visiting the
;; offsets (first vector)
;; offsets:    [1 -1 -3 5] ;; the input
;; increments: [0  0  1 0]
;; steps needed to exit are the sum of all increments

;; TODO how to read the vector using offsets?
;; sum the offsets?

;; while an out-of-bounds exception does not occur
;; do:
;;   read offsets[idx]
;;   make index = offsets[idx] + increments[idx]
;;   increment increments[idx] by one

;; state is: current index
(defn count-steps-to-exit
  [ints]
  (loop [xs ints
         zeros (repeat-zeros (count xs))
         idx 0]
    (when (>= (count xs) idx)
      ;; if oob (< 0 or >= count), return reduce + zeros
      (if (or (< idx 0)
              (>= idx (count xs)))
        (reduce + zeros)
        (recur xs
               (inc-at-idx zeros idx)
               (+ idx
                  (nth xs idx)
                  (nth zeros idx)))))))

;; TODO refactor to take function
(defn count-steps-to-exit-stranger
  [ints]
  (loop [xs ints
         zeros (repeat-zeros (count xs))
         idx 0]
    (when (>= (count xs) idx)
      ;; if oob (< 0 or >= count), return reduce + zeros
      ;;(println idx)
      (if (or (< idx 0)
              (>= idx (count xs)))
        (reduce + zeros)
        (recur xs
               (strange-inc-at-idx zeros idx (nth xs idx))
               (+ idx
                  (nth xs idx)
                  (nth zeros idx)))))))

(deftest day-5-test

   (testing
      (is (= 1 1))
      ;; (is (= (count-steps-to-exit day-5-input-seq) 354121))

;;0) 3  0  1  -3
      (is (= (count-steps-to-exit-stranger '(0 3 0 1 -3)) 1))
      ;; (is (= (count-steps-to-exit-stranger day-5-input-seq) 354121))
      )
   ;; TODO stranger jumps
)
