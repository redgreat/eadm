/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-04-04 23:41:41
 *
 * Module : dashboard.js
 *
 */

function loadChart() {
    let location = document.getElementById("location");
    let finance = document.getElementById("sales");

    $.getJSON('/data/dashboard/', function (resdata) {

        console.log("resdata: " + JSON.stringify(resdata));

        $('#locationWeek').text(resdata[0]);
        $('#stepsWeek').text(resdata[1]);
        $('#sleepWeek').text(resdata[2]);
        $('#heartWeek').text(resdata[3]);

        $('#locationYear').text(resdata[6]);
        $('#financeYear').text(resdata[7]);
        $('#sleepYear').text(resdata[8]);
        $('#heartYear').text(resdata[9]);

        let locationChart = new Chart(location, {
            type: 'line',
            data: {
                labels: ['1月', '2月', '3月', '4月', '5月', '6月', '7月', '8月', '9月', '10月', '11月', '12月'],
                datasets: [{
                    data: resdata[4],
                    backgroundColor: "rgba(48, 164, 255, 0.2)",
                    borderColor: "rgba(48, 164, 255, 0.8)",
                    fill: true,
                    borderWidth: 1
                }]
            },
            options: {
                animation: {
                    duration: 2000,
                    easing: 'easeOutQuart',
                },
                plugins: {
                    legend: {
                        display: false,
                        position: 'right',
                    },
                    title: {
                        display: true,
                        text: 'Number of Visitors',
                        position: 'left',
                    },
                },
            }
        });

        let financeChart = new Chart(finance, {
            type: 'bar',
            data: {
                labels: ['1月', '2月', '3月', '4月', '5月', '6月', '7月', '8月', '9月', '10月', '11月', '12月'],
                datasets: [{
                    label: '收入',
                    data: resdata[5],
                    // data: resdata.finance,
                    backgroundColor: "rgba(76, 175, 80, 0.5)",
                    borderColor: "#6da252",
                    borderWidth: 1,
                }]
            },
            options: {
                animation: {
                    duration: 2000,
                    easing: 'easeOutQuart',
                },
                plugins: {
                    legend: {
                        display: false,
                        position: 'top',
                    },
                    title: {
                        display: true,
                        text: 'Number of Sales',
                        position: 'left',
                    },
                },
            }
        });
    }
)}

loadChart();
