function PlayLow() {
  setTimeout(function(){
   osc.start();
   Tone.Master.volume.rampTo(0, 0.05); //1
   }, 0);

   setTimeout(function(){
     osc.stop();
     Tone.Master.volume.rampTo(-Infinity, 0.05);
   }, 1000);
 }

 function PlayHigh() {
   setTimeout(function(){
    osc2.start();
    Tone.Master.volume.rampTo(0, 0.05); //1
    }, 0);

    setTimeout(function(){
      osc2.stop();
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 1000);
  }

function PlayToneTest() {
   setTimeout(function(){
    osc.start();
    Tone.Master.volume.rampTo(0, 0.05); //1
    }, 0);

    setTimeout(function(){
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 300);

    setTimeout(function(){
      Tone.Master.volume.rampTo(0, 0.05); //2
    }, 600);

    setTimeout(function(){
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 900);

    setTimeout(function(){
      osc.stop();
      osc2.start();
      Tone.Master.volume.rampTo(0, 0.05); //3
    }, 1200);

    setTimeout(function(){
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 1500);

    setTimeout(function(){
      osc.start();
      osc2.stop();
      Tone.Master.volume.rampTo(0, 0.05); //4
    }, 1800);

    setTimeout(function(){
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 2100);

    setTimeout(function(){
      Tone.Master.volume.rampTo(0, 0.05); //5
    }, 2400);

    setTimeout(function(){
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 2700);

    setTimeout(function(){
      osc2.start();
      osc.stop();
      Tone.Master.volume.rampTo(0, 0.05); //6
    }, 3000);

    setTimeout(function(){
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 3300);

    setTimeout(function(){
      Tone.Master.volume.rampTo(0, 0.05); //7
    }, 3600);

    setTimeout(function(){
      osc2.stop();
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 3900);
  }


function PlayTones() {
   setTimeout(function(){
    osc.start();
    Tone.Master.volume.rampTo(0, 0.05); //1
    }, 0);

    setTimeout(function(){
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 300);

    setTimeout(function(){
      eval(my_functions_1[toneMat[0]]);
      eval(my_functions_2[toneMat[0]]);
      Tone.Master.volume.rampTo(0, 0.05); //2
    }, 600);

    setTimeout(function(){
      eval(my_functions_1[toneMat[1]]);
      eval(my_functions_2[toneMat[1]]);
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 900);

    setTimeout(function(){
      eval(my_functions_1[toneMat[2]]);
      eval(my_functions_2[toneMat[2]]);
      Tone.Master.volume.rampTo(0, 0.05); //3
    }, 1200);

    setTimeout(function(){
      eval(my_functions_1[toneMat[3]]);
      eval(my_functions_2[toneMat[3]]);
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 1500);

    setTimeout(function(){
      eval(my_functions_1[toneMat[4]]);
      eval(my_functions_2[toneMat[4]]);
      Tone.Master.volume.rampTo(0, 0.05); //4
    }, 1800);

    setTimeout(function(){
      eval(my_functions_1[toneMat[5]]);
      eval(my_functions_2[toneMat[5]]);
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 2100);

    setTimeout(function(){
      eval(my_functions_1[toneMat[6]]);
      eval(my_functions_2[toneMat[6]]);
      Tone.Master.volume.rampTo(0, 0.05); //5
    }, 2400);

    setTimeout(function(){
      eval(my_functions_1[toneMat[7]]);
      eval(my_functions_2[toneMat[7]]);
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 2700);

    setTimeout(function(){
      eval(my_functions_1[toneMat[8]]);
      eval(my_functions_2[toneMat[8]]);
      Tone.Master.volume.rampTo(0, 0.05); //6
    }, 3000);

    setTimeout(function(){
      eval(my_functions_1[toneMat[9]]);
      eval(my_functions_2[toneMat[9]]);
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 3300);

    setTimeout(function(){
      eval(my_functions_1[toneMat[10]]);
      eval(my_functions_2[toneMat[10]]);
      Tone.Master.volume.rampTo(0, 0.05); //7
    }, 3600);

    setTimeout(function(){
      eval(my_functions_1[toneMat[11]]);
      eval(my_functions_2[toneMat[11]]);
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 3900);

    setTimeout(function(){
      eval(my_functions_1[toneMat[12]]);
      eval(my_functions_2[toneMat[12]]);
      Tone.Master.volume.rampTo(0, 0.05); //8
    }, 4200);

    setTimeout(function(){
      eval(my_functions_1[toneMat[13]]);
      eval(my_functions_2[toneMat[13]]);
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 4500);

    setTimeout(function(){
      eval(my_functions_1[toneMat[14]]);
      eval(my_functions_2[toneMat[14]]);
      Tone.Master.volume.rampTo(0, 0.05); //9
    }, 4800);

    setTimeout(function(){
      eval(my_functions_1[toneMat[15]]);
      eval(my_functions_2[toneMat[15]]);
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 5100);

    setTimeout(function(){
      eval(my_functions_1[toneMat[16]]);
      eval(my_functions_2[toneMat[16]]);
      Tone.Master.volume.rampTo(0, 0.05); //10
    }, 5400);

    setTimeout(function(){
      eval(my_functions_1[toneMat[17]]);
      eval(my_functions_2[toneMat[17]]);
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 5700);

    setTimeout(function(){
      eval(my_functions_1[toneMat[18]]);
      eval(my_functions_2[toneMat[18]]);
      Tone.Master.volume.rampTo(0, 0.05); //11
    }, 6000);

    setTimeout(function(){
      eval(my_functions_1[toneMat[19]]);
      eval(my_functions_2[toneMat[19]]);
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 6300);

    setTimeout(function(){
      eval(my_functions_1[toneMat[20]]);
      eval(my_functions_2[toneMat[20]]);
      Tone.Master.volume.rampTo(0, 0.05); //12
    }, 6600);

    setTimeout(function(){
      eval(my_functions_1[toneMat[21]]);
      eval(my_functions_2[toneMat[21]]);
      Tone.Master.volume.rampTo(-Infinity, 0.05);
    }, 6900);
  }
